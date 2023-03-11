setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ----------
# for comparison with MCMC approach later, we standardize the covariate
Mystd <- function(x) {(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}

Squid$Lat.std <- Mystd(Squid$Lat)
Squid$ML.std  <- Mystd(Squid$ML)



# ------------------------------------------------------------------------------
# Low rank thin-plate regression splines by lme()
# ------------------------------------------------------------------------------

# Calculate sample quantiles
default.knots <- function(x,num.knots) {
  if (missing(num.knots)) num.knots <- max(5,min(floor(length(unique(x))/4),35))
  return(quantile(unique(x), seq(0,1, length= (num.knots+2))[-c(1,(num.knots+2))]))
}


# Function to calculate the low rank thin-plate regression spline
GetZ_LRTP <- function(x, Knots) {
  Z_K            <- (abs(outer(x, Knots,"-")))^3
  OMEGA_all      <- (abs(outer(Knots, Knots,"-")))^3
  svd.OMEGA_all  <- svd(OMEGA_all)
  sqrt.OMEGA_all <-t(svd.OMEGA_all$v %*% (t(svd.OMEGA_all$u)*sqrt(svd.OMEGA_all$d)))
  Z <- t(solve(sqrt.OMEGA_all,t(Z_K)))
  return(Z)
}


# ----------
K <- 5

Knots <- default.knots(Squid$ML.std, K)

Z     <- GetZ_LRTP(Squid$ML.std, Knots)

X     <- model.matrix(~Squid$Lat.std + Squid$ML.std, data = Squid) 


dat1 <- data.frame(y = Squid$d15N, 
                   X = X, 
                   Z = Z, 
                   g = 1)

dat1$g <- factor(dat1$g)



# ----------
library(nlme)


# pdIndent() implements an identity matrix for the covariance matrix of the random effects,
# and therefore ensures that the random effects are independent of one another.
M1 <- lme(y ~ -1 + X, random = list(g = pdIdent(~ Z - 1)), data = dat1)

summary(M1)



# ------------------------------------------------------------------------------
# Extract estimated slopes and random effects
# ------------------------------------------------------------------------------
beta <- M1$coef$fixed

u    <- unlist(M1$coef$random)


# Smoother is obtained by multiplying the third column of X (ML.std) with the slope for ML.std and adding the random effects
f    <- X[,3] * beta[3] + Z %*% u

varf <- X %*% vcov(M1) %*% t(X) 


# ----------
# Smoother
I <- order(Squid$ML.std)

par(mar = c(5,5,2,2))
plot(x=sort(Squid$ML.std), f[I], type = "l", cex.lab = 1.5,
     xlab = "Standardized ML",
     ylab = "Smoother for ML")

sig.eps <- M1$sigma
sig.u   <- sig.eps * exp(unlist(M1$modelStruct))
