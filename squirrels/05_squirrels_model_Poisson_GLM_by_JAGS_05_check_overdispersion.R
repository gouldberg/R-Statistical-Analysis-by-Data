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
# We add block of code for check for overdispersion
#   1.  Within the MCMC algorithm, simulate Poisson data with mean mu[i]
#   2.  Calculate Pearson residuals fro the observed data and also for the new (simulated) data for each observation i.
#   3.  Obtain the sum of squares of the Pearson residuals for the observed data and also for the simulated data
#   4.  Repeat steps 1 through 3 for all MCMC iterations
#   5.  When JAGS is complete, compare the two sum of squares values for each MCMC iteration.
#       If the observed data follows a Poisson distribution, on average the sums of squares of th ePearson residuals for the observed data will be 
#       similar to those of the simulated data.
# ------------------------------------------------------------------------------

X <- model.matrix(~ Ntrees.std + TreeHeight.std + CanopyCover.std, data = SQ2)

K <- ncol(X)

win.data <- list(SqCones = SQ2$SqCones,
                 X       = X,
                 K       = K,
                 N       = nrow(SQ2)
)



# ----------
# model
sink("GLMPoisson2.txt")

cat("
  model{
    #1. Priors for regression coefficients
    for (i in 1:K) { beta[i] ~ dnorm(0,0.0001) } 
    
    #2. Likelihood function
    for (i in 1:N){  
       SqCones[i] ~  dpois(mu[i])
       log(mu[i]) <- eta[i]
       eta[i]     <- inprod(beta[], X[i,]) 
            
       #Discrepancy measures (used for checking overdispersion)
       YNew[i]   ~ dpois(mu[i])   #New data
       expY[i]    <- mu[i] 
       varY[i]    <- mu[i]
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
    beta = rnorm(K, 0, 0.1)
  )  }



# ----------
# Parameters to estimate
params <- c("beta", "PRes", "Fit", "FitNew")



# ----------
# Start Gibbs sampler

K1 <- jags(data       = win.data, 
           inits      = inits, 
           parameters.to.save = params,
           model.file = "GLMPoisson2.txt",
           n.chains   = 3,
           n.iter     = 5000,
           n.thin     = 10,
           n.burnin   = 4000)


K2 <- update(K1, n.iter = 10000, n.thin = 10)



# ----------
# The number of times that the summary statistic of the simulated data is larger than that of the actual data
# provides a Bayesian p-value for the model
# Values close to 0.5 indicate a good model fit, whereas values colose to 0 or 1 indicate problems.

mean(K2$BUGSoutput$sims.list$Fit >  K2$BUGSoutput$sims.list$FitNew) 



# -->
# In all cases, the sum of squared residuals of the model fitted to the actual data differs from that of the simulated data
# Hence we have serious model mis-specification.
# Most likely we have overdispersion


