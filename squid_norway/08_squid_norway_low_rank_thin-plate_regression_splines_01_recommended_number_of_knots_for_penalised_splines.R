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
# Ruppert et al. (2003) recommend using a relatively large number of knots for penalized splines
# ------------------------------------------------------------------------------

# K:  number of knots

# K = min (# of unique covariate values / 4,  35)
# For the squid data, the variable ML has 44 unique values, in which case the above formula suggests using 11 knots for the ML (or ML.std) smoother

( K <- min(length(unique(Squid$ML.std)), 35) )



# ----------
# In our experience using a number of knots according to the formula above works well for Gaussian, Poisson, and binomial models,
# but when more challenging distributions are used in MCMC,
# we suggest reducing the number of knots to as small a number as possible because this tends to improve mixing.

# It also reduces computation time if datasets with more than (approximately) 5,000 observations are used.
K <- 5



# ------------------------------------------------------------------------------
# Plot Z components with specified number of knots (=K)
# ------------------------------------------------------------------------------

# Calculate sample quantiles
default.knots <- function(x,num.knots) {
  if (missing(num.knots)) num.knots <- max(5,min(floor(length(unique(x))/4),35))
  return(quantile(unique(x), seq(0,1, length= (num.knots+2))[-c(1,(num.knots+2))]))
}

Knots <- default.knots(Squid$ML.std, K)

Knots



# -----------
# Show Z components
par(mar = c(5,5,2,2))

plot(y = Squid$d15N, 
     x = Squid$ML.std, 
     xlab = "Standardized ML",
     ylab = "d15N", cex.lab = 1.5,
     type = "n",
     ylim = c(0, 30))
abline(v = as.numeric(Knots), lty= 2)

x <- Squid$ML.std

x1 <- seq(from = min(x), max(x), length = 25)

for (i in 1:K){
  fi <- abs(x1 - Knots[i])^3
  lines(x1, fi)
}


text(1.25,12,"k = 1", cex = 1.2) 
text(1.72,12,"k = 2", cex = 1.2) 
text(2.75, 14,"k = 3", cex = 1.2) 
text(2.7, 10,"k = 4", cex = 1.2) 
text(2.7, 3,"k = 5", cex = 1.2) 

