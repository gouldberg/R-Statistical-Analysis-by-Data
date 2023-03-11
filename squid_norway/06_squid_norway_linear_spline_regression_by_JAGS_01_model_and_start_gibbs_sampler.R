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
# Linear spline regression in JAGS:  model
# ------------------------------------------------------------------------------
probs <- seq(0, 1, length = 5)
QD    <- quantile(Squid$ML.std, probs)
QD


X <- model.matrix(~ Lat.std + ML.std  +  rhs(ML.std, -0.7121410) +  rhs(ML.std, -0.1667513) + rhs(ML.std, 0.6419299), data = Squid)
dim(X)



# ----------
# create a list with all required data for JAGS;
# it contains the response variable Y, the covariate matrix X, the number of covariates (M), and the sample size (N)
win.data <- list(Y = Squid$d15N, X = X, M = ncol(X), N = nrow(Squid))
win.data



# ----------
# model
# Diffuse normal priors are used for the regression parameters.
# For the prior of the square root of the variance of the residuals we use a half Cauchy (25) distribution (num and denom code)
# THe inprod function is used to calculate the expected values of the model.
sink("SquidGAM1.txt")
cat("
    model{
    #1A. Priors regression parameters
    for (i in 1:M) { beta[i] ~ dnorm(0,0.0001) }
    
    #1B. Prior for variance for epsilon
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma <- abs(num / denom) 
    tau   <- 1 / (sigma * sigma)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]  ~ dnorm(mu[i], tau)
    mu[i] <- inprod(X[i,], beta[])
    
    #3. Discrepancy measures   
    E[i] <- Y[i] - mu[i]
    }     
    }
    ",fill = TRUE)
sink()



# ----------
# Inits function
# For the prior of the square root of the variance of the residuals
# we use a half-Cauchy (25) distribution (num and denom)

inits  <- function () {
  list(beta  = rnorm(ncol(X), 0, 0.01),  #Regression parameters
       num   = rnorm(1, 0, 25),          #Prior stuff for variance epsilon
       denom = rnorm(1, 0, 1)            #Prior stuff for variance epsilon
  )  }




# ----------
# Parameters to estimate
params <- c("beta", "E", "sigma")



# ------------------------------------------------------------------------------
# Linear spline regression in JAGS:  start Gibbs sampler
# ------------------------------------------------------------------------------
library(R2jags)

K1   <- jags(data       = win.data,
             inits      = inits,
             parameters = params,
             model      = "SquidGAM1.txt",
             n.thin     = 10,
             n.chains   = 3,
             n.burnin   = 14000,
             n.iter     = 15000)

K1

K2 <- update(K1, n.iter = 20000) 


print(K2, digits = 2)



