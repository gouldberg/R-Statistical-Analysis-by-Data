# ------------------------------------------------------------------------------
# Data: IrishPh
#
# simple linear regression by MCMC jags and INLA (BASICS)
# ------------------------------------------------------------------------------
rm(list=ls())

setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "lattice", "ggplot2", "sp", "gstat", "ggmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source("./ZuurSpatialTemporalAndSpatialTemporalEcologicalDataAnalysiswithRINLA/MCMCSupportHighstatV4.R")

# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.table(file = "./ZuurSpatialTemporalAndSpatialTemporalEcologicalDataAnalysiswithRINLA/IrishPh.txt", header=T, dec=".")
dim(data)
str(data)

Hmisc::describe(data)

is.na(data)
colSums(is.na(data))


# ------------------------------------------------------------------------------
# Standardize covariate (SDI)
# ------------------------------------------------------------------------------
data$SDI.std <- Mystd(data$SDI)
M1 <- lm(pH ~ SDI.std, data = data)
summary(M1)


# ------------------------------------------------------------------------------
# JAGS data
# ------------------------------------------------------------------------------
X <- model.matrix(~ 1 + SDI.std, data = data)
K <- ncol(X)

JAGS.data <- list(Y = data$pH,      #Response
                  X = X, #Covariate
                  K = K,
                  N = nrow(data))   #Sample size
JAGS.data


# ------------------------------------------------------------------------------
# JAGS modelling code
# ------------------------------------------------------------------------------
sink("IrishPh_JAGS.txt")
cat(" 
    model{
    #1A. Diffuse Normal priors beta and sigma
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)} #Precision!!
    
    #Diffuse uniform prior for sigma
    tau  <- 1 / (sigma * sigma)
    sigma ~ dunif(0, 20)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]  ~ dnorm(mu[i], tau)  
    mu[i] <- inprod(beta, X[i,])
    }
    }           
    ",fill = TRUE)
sink()


# ------------------------------------------------------------------------------
# Initial values & parameters to save
# Run JAGS
# ------------------------------------------------------------------------------
inits  <- function () {
  list(
    beta  = rnorm(K, mean = 0, sd = 0.1),
    sigma = runif(1, 0, 20))}

params <- c("beta", "sigma")

library(R2jags)
J1 <- jags(data       = JAGS.data,
           inits      = inits,
           parameters = params,
           model      = "IrishPh_JAGS.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

# posterior distribution sample: 50000 / 10 * 3(n.chains) = 15,000
J2  <- update(J1, n.iter = 50000, n.thin = 10)  
out <- J2$BUGSoutput
print(out, digits = 3)  


# ------------------------------------------------------------------------------
# Assess mixing --> good mixing
# ------------------------------------------------------------------------------
MyNames <- c("Intercept", "Slope", "sigma")
MyBUGSChains(out,  c("beta[1]", "beta[2]", "sigma"), PanelNames = MyNames)


# ------------------------------------------------------------------------------
# Present statistics:  posterior information
# ------------------------------------------------------------------------------
MyNames <- c("Intercept", "Slope", "sigma")
OUT1 <- MyBUGSOutput(out, c("beta[1]", "beta[2]", "sigma"), VarNames = MyNames)
print(OUT1, digits = 5)

# Posterior distributions
MyBUGSHist(out, c("beta[1]", "beta[2]", "sigma"), PanelNames = MyNames)



# ------------------------------------------------------------------------------
# INLA (Integrated nested Laplace approximation)

# as to "family" argument, see
# names(inla.models()$likelihood)
# ------------------------------------------------------------------------------
library(INLA)
names(inla.models()$likelihood)

I1 <- inla(pH ~ SDI.std, data = data, family = "gaussian") 
print(summary(I1), digits = 3)


# Fixed effects: posterior mean, sd, credible interval
Betas <- I1$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Betas, digits = 2)


# Calculate fitted value
X   <- model.matrix(~ 1 + SDI.std, data = data)
Fit <- X %*% Betas[,1]
E1  <- data$pH - Fit


# specify "control.predictor = list(compute = TRUE)" to calculate fitted value
I2 <- inla(pH ~ SDI.std, data = data, family = "gaussian", control.predictor = list(compute = TRUE)) 
Fit2 <- I2$summary.fitted.values[,"mean"] 
cbind(Fit, Fit2)


# Hyper parameter: The posterior mean of the Precision for the Gaussian observartions
I1$summary.hyperpar
# Hyper parameter: The residual variance
1 / sqrt(I2$summary.hyperpar[,1])


# Posterior marginal distribution
I1$marginals.fixed
I1$marginals.hyperpar

pmbeta1 <- I2$marginals.fixed$`(Intercept)`
pmbeta2 <- I2$marginals.fixed$SDI.std
pmtau   <- I2$marginals.hyperpar$`Precision for the Gaussian observations`
pm.sigma <- inla.tmarginal(function(x) sqrt(1/x), pmtau)

par(mfrow = c(2, 2), cex.lab = 1.5, mar = c(5,5,2,2))
plot(x = pmbeta1[,1], y = pmbeta1[,2], type = "l", xlab = expression(beta[1]), ylab = expression(paste("P(", beta[1] ," | Data)")))
text(7.17, 15, "A", cex = 1.5)

plot(x = pmbeta2[,1], y = pmbeta2[,2], type = "l", xlab = expression(beta[2]), ylab = expression(paste("P(", beta[2] ," | Data)")))
text(-0.67, 15, "B", cex = 1.5)

plot(x = pmtau[1:75,1],  y = pmtau[1:75,2], type = "l", xlab = expression(tau), ylab = expression(paste("P(", tau ," | Data)")))
text(3, 0.6, "C", cex = 1.5)

plot(x = pm.sigma[,1], y = pm.sigma[,2], type = "l", xlab = expression(sigma), ylab = expression(paste("P(", sigma ," | Data)")))
text(0.329, 22, "D", cex = 1.5)


# Posterior mean value of sigma
inla.emarginal(function(x) sqrt(1/x), pmtau)

# mean, sd, quantiles of the posteriror marginal distribution of beta2, credible interval
inla.zmarginal(pmbeta2)
inla.qmarginal(c(0.025, 0.975), pmbeta2)

# highest posterial density interval of beta2
inla.hpdmarginal(0.95, pmbeta2) 










