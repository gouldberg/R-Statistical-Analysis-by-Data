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
# Low rank thin-plate regression splines in JAGS:  model
# ------------------------------------------------------------------------------

# Get the X and Z for the tprs
# Knots:
default.knots <- function(x,num.knots) {
  if (missing(num.knots)) num.knots <- max(5,min(floor(length(unique(x))/4),35))
  return(quantile(unique(x), seq(0,1, length= (num.knots+2))[-c(1,(num.knots+2))]))
}


# Function to calculate the low rank thin plate regression spline
GetZ_LRTP <- function(x, Knots) {
  Z_K            <- (abs(outer(x, Knots,"-")))^3
  OMEGA_all      <- (abs(outer(Knots, Knots,"-")))^3
  svd.OMEGA_all  <- svd(OMEGA_all)
  sqrt.OMEGA_all <-t(svd.OMEGA_all$v %*% (t(svd.OMEGA_all$u)*sqrt(svd.OMEGA_all$d)))
  Z <- t(solve(sqrt.OMEGA_all,t(Z_K)))
  return(Z)
}


Xcov <- model.matrix(~Lat.std, data = Squid)    


K <- 5

( Knots <- default.knots(Squid$ML.std, K) )

Z <- GetZ_LRTP(Squid$ML.std, Knots)

N <- nrow(Squid)

X <- Squid$ML.std


win.data2 <- list(Y            = Squid$d15N,          #Response variable
                  Xcov         = Xcov,                #Covariates  
                  N            = nrow(Squid),         #Sample size
                  M            = ncol(Xcov),          #Number of betas
                  Xspl         = X,                   #Basis for smoother 
                  Zspl         = Z,
                  Mz           = ncol(Z)              #Number of us
)


win.data2



# ----------
# model
sink("GAMlrtprs.txt")

cat("
model{
    #1A. Priors regression parameters
    for (i in 1:M)  {beta[i] ~ dnorm(0, 0.0001) }  
    for (i in 1:Mz) {u[i] ~ dnorm(0, tau.u )  }
    b ~ dnorm(0, 0.0001)
   
    #1B. Prior for variance for epsilon
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma <- abs(num / denom) 
    tau   <- 1 / (sigma * sigma)
       
    #Priors for variance random intercept u for smoother
    num.u   ~ dnorm(0, 0.0016) 
    denom.u ~ dnorm(0, 1)
    sigma.u <- abs(num.u / denom.u) 
    tau.u   <- 1 / (sigma.u * sigma.u)
     
    #2. Likelihood
    for (i in 1:N) {
       Y[i]  ~  dnorm(mu[i], tau)
       mu[i] <- inprod(beta[], Xcov[i,]) + F1[i]         
       F1[i] <- Xspl[i] * b + inprod(Zspl[i,], u[])  #Smoother
              
       #3. Discrepancy measures   
       E[i] <- Y[i] - mu[i]
    }     
 }
",fill = TRUE)

sink()



# ----------
# Inits function

inits2  <- function () {
  list(beta    = rnorm(ncol(Xcov), 0, 0.1),#Regression parameters
       b       = rnorm(1, 0, 0.1),         #Regression terms smoother
       num     = rnorm(1, 0, 25),          #Prior stuff for variance epsilon
       denom   = rnorm(1, 0, 1),           #Prior stuff for variance epsilon
       num.u   = rnorm(1, 0, 25),          #Prior stuff for variance u
       denom.u = rnorm(1, 0, 1),           #Prior stuff for variance u
       u       = rnorm(K, 0, 1)
  )  }



# ----------
# Parameters to estimate
params2 <- c("beta", "E", "sigma", "b", "u", "sigma.u")



# ------------------------------------------------------------------------------
# Low rank thin-plate regression splines in JAGS:  Start gibbs sampler
# ------------------------------------------------------------------------------
K1   <- jags(data       = win.data2,
             inits      = inits2,
             parameters = params2,
             model      = "GAMlrtprs.txt",
             n.thin     = 10,
             n.chains   = 3,
             n.burnin   =  15000,
             n.iter     = 115000)


# K2 <- update(K1, n.iter = 100000) 

print(K1, digits = 2)



# ------------------------------------------------------------------------------
# B-splines in JAGS:  assess model
#   - chain mixing
#   - ACF per chain
#   - parameter distribution
# ------------------------------------------------------------------------------

out <- K1$BUGSoutput

MyBUGSChains(out, c(uNames("beta", ncol(Xcov))))

MyBUGSChains(out, c("b", uNames("u", ncol(Z))))

MyBUGSChains(out, c("sigma", "sigma.u"))


# #MyBUGSACF(out, uNames("beta", K))
# MyBUGSHist(out, uNames("beta", K))



# ------------------------------------------------------------------------------
# Low rank thin-place splines in JAGS:  Sketch smoothers
# ------------------------------------------------------------------------------

# Extract coefficients
b <- out$sims.list$b  #ML smoother
u <- out$sims.list$u


#This depends on the selected data!
range(Squid$ML.std)
ML.100 <- seq( -1.4,  3, length = 100)


# Convert this covariate into a smoother basis
Z100 <- GetZ_LRTP(ML.100, Knots)


# Calculate the smoothers
f1 <-  ML.100 %*% t(b) + Z100 %*% t(u) 


# Get posterior mean and 95% credible intervals
f1.info <- MySmoother(f1)



# ----------
# Plot the smoothers
par(mar = c(5,5,2,2))  
plot(x = ML.100, 
     y = f1.info[,4], 
     type = "l", 
     xlab = "Standardized ML",
     ylab = "Smoother",
     ylim = c(-2,2), cex.lab = 1.5)
lines(ML.100, f1.info[,1], lty=2)
lines(ML.100, f1.info[,3], lty=2)
