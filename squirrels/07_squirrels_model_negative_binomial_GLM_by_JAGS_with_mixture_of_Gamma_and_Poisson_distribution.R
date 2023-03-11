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
# Fitting negative binomial GLM model in JAGS with mixture of Gamma and Poisson distribution
# ------------------------------------------------------------------------------

sink("GLMNB3.txt")

cat("
    model{
    
    #Priors for regression coefficients
    for (i in 1:K) {
    beta[i] ~ dnorm(0,0.0001)
    } 
    #Prior for size
    size ~ dunif(0.001, 5)
    
    #Likelihood function
    for (i in 1:N){  
    SqCones[i]   ~ dpois(g[i])
    g[i]         ~ dgamma(size, rateParm[i])
    rateParm[i] <- size / mu[i] 
    log(mu[i])  <- eta[i]
    eta[i]      <- inprod(beta[],X[i,])
    
    #Discrepancy measures (used for checking overdispersion)
    YNew[i]   ~ dpois(g[i])   #New data
    expY[i]    <- mu[i] 
    varY[i]    <- mu[i] + pow(mu[i],2) / size
    PRes[i]    <- (SqCones[i]  - expY[i]) / sqrt(varY[i])
    PResNew[i] <- (YNew[i] - expY[i]) / sqrt(varY[i])
    D[i]       <- pow(PRes[i], 2)
    DNew[i]    <- pow(PResNew[i], 2)
    }    
    
    Fit         <- sum(D[1:N])
    FitNew      <- sum(DNew[1:N])
    }
    ",fill = TRUE)

sink()



# ----------
# Set the initial values for the betas and sigma
inits <- function () {
   list(
    beta       = rnorm(K, 0, 0.1),
    size         = runif(0.001,5)
       )  }



# ----------
# Parameters to estimate
params <- c("beta", "Fit", "FitNew", "size")



# ----------
K1 <- jags(data = win.data, 
           inits = inits, 
           parameters.to.save = params,
           model.file = "GLMNB3.txt",
           n.chains = 3,
           n.iter = 5000,
           n.thin = 10,
           n.burnin = 4000)



K2 <- update(K1, n.iter = 10000, n.thin = 10)           



