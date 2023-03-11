setwd("//media//kswada//MyFiles//R//squirrels")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Red Squirrels
# ------------------------------------------------------------------------------

SQ <- read.table(file = "RedSquirrels.txt", header = TRUE, dec = ".")


str(SQ)


# ----------
# remove large DBH
SQ2 <- subset(SQ, DBH < 0.6)



# ------------------------------------------------------------------------------
# Scaling covariate  (to compare with MCMC result)
# ------------------------------------------------------------------------------

SQ2$Ntrees.std      <- as.numeric(scale(SQ2$Ntrees))

SQ2$TreeHeight.std  <- as.numeric(scale(SQ2$TreeHeight))

SQ2$CanopyCover.std <- as.numeric(scale(SQ2$CanopyCover))



# ------------------------------------------------------------------------------
# Fitting Poisson GLM model in JAGS
# ------------------------------------------------------------------------------

# create a list with all required data for JAGS;
# it contains the response variable Y, the covariate matrix X, the number of covariates (M), and the sample size (N)

X <- model.matrix(~ Ntrees.std + TreeHeight.std + CanopyCover.std, data = SQ2)

K <- ncol(X)

win.data <- list(SqCones = SQ2$SqCones,
                 X       = X,
                 K       = K,
                 N       = nrow(SQ2)
)



# ----------
# model
# We specify diffuse priors for all 4 regression parameters (1 intercept and 3 slopes)
# We use a normal distribution with mean 0 and variance 100^2
# In JAGS functions the variation is specified as precision, defined by 1 / variance
# inprod() function in JAGS multiplies the 4 betas by the four columns in X for each row.

sink("GLMPoisson.txt")

cat("
  model{
    #1. Priors for regression coefficients
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001) } 
  
    #2. Likelihood function
    for (i in 1:N){  
       SqCones[i] ~  dpois(mu[i])
       log(mu[i]) <- eta[i]
       eta[i]     <- inprod(beta[], X[i,]) 
    }    
  }
",fill = TRUE)

sink()



# ----------
# Inits function
# We draw 4 random values from a normal distribution with mean 0 and standard deviation 0.1

inits <- function () {
  list(
    beta = rnorm(K, 0, 0.1)
  )  }



# ----------
# Parameters to estimate
# We store the chanis of the four parameters
params <- c("beta")




# ------------------------------------------------------------------------------
# Fitting Poissson GLM in JAGS
# start Gibbs sampler
# ------------------------------------------------------------------------------

library(R2jags)

K1 <- jags(data       = win.data, 
           inits      = inits, 
           parameters.to.save = params,
           model.file = "GLMPoisson.txt",
           n.chains   = 3,
           n.iter     = 5000,
           n.thin     = 10,
           n.burnin   = 4000)


print(K1, intervals = c(0.025, 0.975), digits = 3)



# -->
# As a result, we obtain 300 MCMC iterations (= 3 * (5000 - 4000) / 10) for the posterior distribution of each parameter.
# This is not enough !!



# ----------
# If there are not error messages and you are sure everything is ok, then run the update function with a considerable large number of iterations.
# JAGS will continue from the point where K1 stopped.
K2 <- update(K1, n.iter = 10000, n.thin = 10)           


print(K2, intervals = c(0.025, 0.975), digits = 3)


# -->
# Columns mu.vect and sd.vect represent the posterior means and standard deviations.
# Column Rhat is a diagnostic tool for assessing mixing of the chains.
# Rhat is the value of the Gelman-Rubin statistic. Gelman (1996) suggests that a value lower than 1.1 or 1.2 is acceptable.
# Values close to 1 indicate good mixing, although a visual inspection of the chains may be more informative.


# ----------
# 3000 MCMC iterations for beta[1], beta[2], beta[3], beta[4] and deviance
head(K2$BUGSoutput$sims.matrix)



# ----------
# The sims.array object shows the three chains for each variable as columns
head(K2$BUGSoutput$sims.array[,,"beta[1]"])

