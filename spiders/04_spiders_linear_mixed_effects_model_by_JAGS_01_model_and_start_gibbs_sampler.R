setwd("//media//kswada//MyFiles//R//spiders")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Spiders
# ------------------------------------------------------------------------------

Spiders <- read.table(file = "Spiders.txt", header = TRUE, dec = ".")


str(Spiders)



# ----------
# Some plots are dropped from the analysis
Spiders$fPlot <- factor(Spiders$Plot)

Spiders <- Spiders %>% filter(! fPlot %in% c("4", "9", "11", "14", "23"))

Spiders$fPlot <- as.factor(as.numeric(Spiders$fPlot))



# ------------------------------------------------------------------------------
# Standardizing covariate (to compare with MCMC result)
# ------------------------------------------------------------------------------

# Add na.rm = TRUE if need to deal with NAs
MyNorm <- function(x){ (x - mean(x)) / sd(x) }

Spiders$HerbLayerc <- MyNorm(Spiders$HerbLayer)
Spiders$GroundVegc <- MyNorm(Spiders$GroundVeg)
Spiders$Litterc    <- MyNorm(Spiders$Litter)



# ------------------------------------------------------------------------------
# Fitting a linear regression mixed effects model in JAGS
# ------------------------------------------------------------------------------

X <- model.matrix(~ HerbLayerc + GroundVegc + Litterc, data = Spiders)

K   <- ncol(X)

re  <- as.numeric(Spiders$fPlot)

Nre <- length(unique(Spiders$fPlot))



# ----------
# create a list with all required data for JAGS;
# it contains the response variable Y, the covariate matrix X, the number of covariates (M), and the sample size (N)
win.data1 <- list(Y       = Spiders$Hlog10,
                  X       = X,
                  K       = K,
                  N       = nrow(Spiders),
                  re      = re,
                  Nre  = Nre)
win.data1



# ----------
# model
# Diffuse normal priors are used for the regression parameters.
# The inprod function is used to calculate the expected values of the model.

sink("lmm.txt")

cat("
model {
    #1. Weak priors for regression parameters
    for (i in 1:K){ beta[i] ~ dnorm(0, 0.0001) } 

    #Priors for random effect plot
    for (i in 1:Nre) { a[i] ~ dnorm(0,  tau.plot ) }

    #Priors for the two sigmas  --> model as precision
    tau.plot <- 1 / (sigma.plot * sigma.plot)
    tau.eps  <- 1 / (sigma.eps * sigma.eps)
    sigma.plot ~ dunif(0.001, 10)
    sigma.eps  ~ dunif(0.001, 10)
    
    #2. Likelihood of the data
    for (i in 1:N) {
        Y[i]  ~  dnorm(mu[i], tau.eps)
        mu[i]  <- eta[i] 
        eta[i] <- inprod(beta[], X[i,]) + a[re[i]]
      }
    }
",fill = TRUE)

sink()



# ----------
# Inits function
# For the prior of the square root of the variance of the residuals

inits1 <- function () {
  list(beta       = rnorm(K, 0, 0.01),
       a          = rnorm(Nre, 0, 1),
       sigma.eps  = runif(1, 0.001, 10),
       sigma.plot = runif(1, 0.001, 10)
  )}



# ----------
# Parameters to estimate
params1 <- c("beta", "a", "sigma.plot", "sigma.eps")



# ------------------------------------------------------------------------------
# Fitting a linear regression mixed effects model in JAGS
# start Gibbs sampler
# ------------------------------------------------------------------------------
library(R2jags)

J0 <- jags(data = win.data1,
           inits = inits1,
           parameters = params1,
           model.file = "lmm.txt",
           n.thin   = 10,
           n.chains = 3,
           n.burnin = 4000,
           n.iter   = 5000)


J1 <- update(J0, n.iter = 10000, n.thin= 10)

print(J1, digits = 2)



