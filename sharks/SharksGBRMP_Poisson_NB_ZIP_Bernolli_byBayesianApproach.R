# ------------------------------------------------------------------------------
# Data:  SharksGBRPM
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "lattice", "ggplot2", "MASS", "VGAM", "broom")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.table(file = "./ZuurZero-InflatedModelswithR/SharksGBRMP.txt", header=T, dec=".")
dim(data)
str(data)

Hmisc::describe(data)

# SoakTime is sampling effort
# we use SoakTime transformed by natural log:  due to the exponential relationship between the expected number of TGS and the covariates
data$LogSoakTime <- log(data$SoakTime)


# ------------------------------------------------------------------------------
# Standardize covariates for securing good mixing
# ------------------------------------------------------------------------------
MyStd <- function(x) { (x - mean(x)) / sd(x)}
data$DistReef.std    <- MyStd(data$DistReef)
data$HardCoral.std   <- MyStd(data$HardCoral)
data$LogSoakTime.std <- MyStd(data$LogSoakTime)


# ------------------------------------------------------------------------------
# TGS Poisson:
#  - Prepare data for JAGS
#  - JAGS modelling code
# ------------------------------------------------------------------------------
# Prepare data for JAGS
X <- model.matrix(~ 1 + DistReef.std + HardCoral.std + Zoning  + LogSoakTime.std, data = data)
K <- ncol(X)
JAGS.data <- list(Y = data$TGS, 
                  X = X,
                  K = K,
                  N = nrow(data))
JAGS.data


# JAGS modelling code
sink("SharksGBRMP_TGS_SharksPoisson.txt")
cat(" 
    model{
    #1. Priors beta
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001) }
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]        ~ dpois(mu[i])  
    log(mu[i]) <- inprod(beta[], X[i,])  
    }
    
    #3. Pearson residuals
    for (i in 1:N){
    ExpY[i] <- mu[i] 
    VarY[i] <- mu[i]
    E[i]   <- (Y[i]  - ExpY[i]) / sqrt(VarY[i])    
    }
    
    #Simulated data with mean/variance taken from the fitted model 
    for (i in 1:N){
    YNew[i] ~  dpois(mu[i])                      
    ENew[i] <- (YNew[i] - ExpY[i]) / sqrt(VarY[i]) 
    D[i]    <- E[i]^2                      
    DNew[i] <- ENew[i]^2   
    }          
    
    #Sum of squared Pearson residuals:
    Fit     <- sum(D[1:N])      
    FitNew  <- sum(DNew[1:N])   
    }           
    ",fill = TRUE)

sink()


# ------------------------------------------------------------------------------
# TGS Poisson:
#  - Initial values and parameters to save
#  - Start JAGS
# ------------------------------------------------------------------------------
library(R2jags)

# Initial values and parameters to save
inits  <- function () {
  list(
    beta  = rnorm(K, 0, 0.1))}

params <- c("beta", "ExpY", "E", "Fit", "FitNew", "YNew")


# Start JAGS
P1 <- jags(data       = JAGS.data,
           inits      = inits,
           parameters = params,
           model      = "SharksGBRMP_TGS_SharksPoisson.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

# P1 is considered as burn-in.
# now P2 iterations = ( 5000 / 10 ) * 3(n.chains) = 15,000  --> required number of iterations at minimum
P2  <- update(P1, n.iter = 50000, n.thin = 10)  

# need more iterations ... see MyBUGSChains
P2  <- update(P2, n.iter = 50000, n.thin = 10)  
out <- P2$BUGSoutput
print(out, digits = 3)  


# ------------------------------------------------------------------------------
# TGS Poisson:
#  - Assess mixing
#  - Present output
# ------------------------------------------------------------------------------
source("./ZuurZero-InflatedModelswithR/MCMCSupportHighstatV4.R")


# Assess mixing
MyNames <- colnames(X)
MyBUGSChains(out, c(uNames("beta", K)), PanelNames = MyNames)

MyBUGSACF(out, c(uNames("beta", K)), PanelNames = MyNames)


# Present output:  Posterior means and 95% credible intervals
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K)), VarNames = MyNames)
print(OUT1, digits = 4)

MyBUGSHist(out, c(uNames("beta", K)), PanelNames = MyNames)


# ------------------------------------------------------------------------------
# TGS Poisson:
#  - Check overdispersion and zero counts
#
# Poisson GLM is capable of dealing with the relatively large number of zeros in the TGS data, 
# but the observed data indicates (marginal) overdispersion against this model.
# ------------------------------------------------------------------------------
# Check for overdispersion (Frequentist way of thinking, basis in a chi-square thype test) --> 1.44
E    <- out$mean$E
N    <- nrow(data)
p    <- K
sum(E^2) / (N - p)


# The comparison of the sum of squared Pearson residuals
# In 92.2% of the simulated datasets, the sum of squared Pearson residuals is larger than that of the simulated data.  -->  indication of (marginal) overdispersion.
mean(out$sims.list$Fit > out$sims.list$FitNew)


# Frequency plots for the origianl and 8 simulated datasets from Poisson GLM
# Note that the simulated data from the Poisson GLM tends to have a similar number of zeros as compared to the observed number of TGS zeros at the 485 sites,
# but the range of the simulated data is large !!
dim(out$sims.list$YNew)
YNew <- out$sims.list$YNew
par(mfrow = c(3,3), mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(data$TGS), xlab = "Values", ylab = "Frequencies", main = "Observed data")
plot(table(YNew[1,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 1")
plot(table(YNew[2,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 2")
plot(table(YNew[3,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 3")
plot(table(YNew[4,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 4")
plot(table(YNew[5,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 5")
plot(table(YNew[6,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 6")
plot(table(YNew[7,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 7")
plot(table(YNew[8,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 8")


# Average frequency from simulated dataset versus original observed data
Z1 <- table(YNew[1,])
Z2 <- table(YNew[2,])

Z <- matrix(nrow = 100, ncol = 15000)
Z[,] <- NA
for (i in 1:15000) {
  Zi <- table(YNew[i,])	
  Z[as.numeric(names(Zi)) + 1, i] <- Zi
}

par(mfrow=c(1,1))
AverageTable <- rowSums(Z, na.rm = TRUE) / rowSums(!is.na(Z))
AverageTable[AverageTable == "NaN"] <- 0
AverageTable
Xi <- 0:49
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Xi, y = AverageTable[1:50], type = "h", ylim = c(0, 500), lwd = 2, xlab = "TGS values", ylab = "Frequencies")

Zs <- table(data$STS)
nx <- length(Zs)
NamesZ <- as.numeric(names(Zs))
for (i in 1:nx) segments(x0 = NamesZ[i] + 0.4, x1 = NamesZ[i] + 0.4, y0 = 0, y1 = Zs[i], lwd = 7)


# How often the predicted number of zeros in the 15,000 simulated datasets is larger than in the original data  --> 61.8%
# If this is about 50% then we can conclude that the number of zeros in the observed data complies with what can be expected from the Poisson GLM.
# Hence the Poisson GLM is capable of dealing with the relatively large number of zeros in the TGS data.
YNewSmaller <- vector(length = 15000)
for (i in 1:15000) YNewSmaller[i] <- sum(YNew[i,]==0) < sum(data$TGS==0)
sum(YNewSmaller) / 15000


# ------------------------------------------------------------------------------
# TGS Poisson:
#  - Model validation:  Posterior mean Pearson residuals versus posterior mean fitted values and all continuous covariates in the model
#
# There are no clear outliers or non-linear patterns, but some of the residuals patterns look odd and require further investigation.
# ------------------------------------------------------------------------------
ExpY <- out$mean$ExpY
par(mfrow = c(2,2), mar = c(5, 5, 2, 2), cex.lab = 1.5)

plot(x = ExpY, y = E, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = data$DistReef, y = E, xlab = "DistReef", ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = data$HardCoral, y = E, xlab = "HardCoral", ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = data$LogSoakTime, y = E, xlab = "LogSoakTime", ylab = "Pearson residuals")
abline(h = 0, lty = 2)


# ------------------------------------------------------------------------------
# TGS Poisson:
#  - Model validation:  Posterior mean Pearson residuals versus spatial positions
#
# We are not entirely sure about the patterns:  the big residuals are all in the lowe east part of the study area
# --> may be wise to make a variogram of the residuals to check spatial independence more formally ...
# ------------------------------------------------------------------------------
# The size of the points are proportional to the value of the residuals
par(mfrow = c(1,1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
xyplot(Lat ~ Lon,
       col = 1,
       pch = 16,
       cex = 3 * sqrt(2 * abs(E) / max(abs(E))),
       data = data,
       xlab = list(label = "Longitude", cex = 1.5),
       ylab = list(label = "Latitude", cex = 1.5))


# ------------------------------------------------------------------------------
# Total number of sharks:  Negative binomial GLM
# ------------------------------------------------------------------------------
data$TA <- rowSums(data[,1:23])
JAGS.data <- list(Y = data$TA, X = X, K = K, N = nrow(data))

# This is the JAGS code for a NB GLM
sink("SharksGBRMP_TA_dataJAGS.txt")
cat("
    model{
    #1A. Priors beta 
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}
    
    #1B Priors for k (size)
    size ~ dunif(0, 20)
    
    #2. Likelihood
    for (i in 1:N) {
    #This is just how JAGS implements a NB distribution
    #It is based on general mathematical rules for a NB 	
    Y[i] ~  dnegbin(p[i], size)
    p[i] <- size / (size + mu[i])  
    
    log(mu[i]) <- eta[i]
    eta[i]     <- inprod(beta[], X[i,]) 
    
    #3. Discrepancy measures 
    #Pearson residuals
    Exp[i] <- mu[i] 
    Var[i] <- mu[i] + mu[i] * mu[i] / size
    E[i]   <- (Y[i]  - Exp[i]) / sqrt(Var[i])    
    
    #Simulated data with mean/variance taken from the fitted model 
    #See text under block B, below.    
    YNew[i] ~  dnegbin(p[i], size)                      
    
    #Pearson residual for predicted data      
    ENew[i] <- (YNew[i] - Exp[i]) / sqrt(Var[i]) 
    
    #Squared residuals
    D[i]    <- pow(E[i], 2)                      
    DNew[i] <- pow(ENew[i], 2)   
    }          
    
    #Sum of squared Pearson residuals:
    Fit     <- sum(D[1:N])      
    #Sum of squared predicted Pearson residuals:     
    FitNew  <- sum(DNew[1:N])   
    }
    ",fill = TRUE)

sink()

# Initial values & parameters to save
inits  <- function () {
  list(
    beta = rnorm(K, 0, 0.1),
    size = runif(1, 0, 20 ) )  }

params <- c("beta", "E", "mu", "Fit", "FitNew", "size", "YNew")

# Start JAGS
# !!!  It takes time ....
G1 <- jags(data       = JAGS.data,
           inits      = inits,
           parameters = params,
           model      = "SharksGBRMP_TA_dataJAGS.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)
# G1 is considered as burn-in.
# now G2 iterations = ( 5000 / 10 ) * 3(n.chains) = 15,000  --> required number of iterations at minimum
G2   <- update(G1, n.iter = 50000, n.thin = 10)  
outG <- G2$BUGSoutput


# Assess mixing --> looks better mixing than Poisson GLM
MyNames <- c(colnames(X), "k")
MyBUGSChains(outG, c(uNames("beta", K), "size"), PanelNames = MyNames)


# Present output: Posterior means, standard errors and 95% credible intervals
# NOTE that all parameters are important !!!
OUT1 <- MyBUGSOutput(outG, c(uNames("beta", K), "size"), VarNames = MyNames)
print(OUT1, digits = 4)

MyBUGSHist(outG, c(uNames("beta", K), "size"), PanelNames = MyNames)


# Assess Overdispersion
# Option 1 --> 1.10, indicating no overdispersion
E    <- outG$mean$E
N    <- nrow(data)
p    <- K + 1
sum(E^2) / (N - p)


# Option 2 --> in 73% of the cases the sum of squared Pearson residuals for the original data is larger than that of the simulated data... not too bad
mean(outG$sims.list$Fit > outG$sims.list$FitNew)


# Simulate from the model
YNew <- outG$sims.list$YNew
par(mfrow = c(3,3), mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(data$TA), xlab = "Values", ylab = "Frequencies", main = "Observed data")
plot(table(YNew[1,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 1")
plot(table(YNew[2,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 2")
plot(table(YNew[3,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 3")
plot(table(YNew[4,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 4")
plot(table(YNew[5,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 5")
plot(table(YNew[6,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 6")
plot(table(YNew[7,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 7")
plot(table(YNew[8,]), xlab = "Values", ylab = "Frequencies", main = "MCMC 8")


# Average frequency plot versus original TA counts
Z1 <- table(YNew[1,])
Z2 <- table(YNew[2,])
Z1
Z2

Z <- matrix(nrow = 100, ncol = 15000)
Z[,] <- NA

for (i in 1:15000) {
  Zi <- table(YNew[i,])	
  Z[as.numeric(names(Zi)) + 1, i] <- Zi
}
Z[1,]

AverageTable <- rowSums(Z, na.rm = TRUE) / rowSums(!is.na(Z))
AverageTable[AverageTable == "NaN"] <- 0
AverageTable
Xi <- 0:49
par(mfrow=c(1,1))
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Xi, y = AverageTable[1:50], type = "h", ylim = c(0, 500), lwd = 2, xlab = "TA values", ylab = "Frequencies")

Zs <- table(data$TA)
nx <- length(Zs)
NamesZ <- as.numeric(names(Zs))
for (i in 1:nx) segments(x0 = NamesZ[i] + 0.4, x1 = NamesZ[i] + 0.4, y0 = 0, y1 = Zs[i], lwd = 7)


# How often the predicted number of zeros in the 15,000 simulated datasets is larger than in the original data  --> 46.3%
# If this is about 50% then we can conclude that the number of zeros in the observed data complies with what can be expected from the Poisson GLM.
YNewSmaller <- vector(length = 15000)
for (i in 1:15000) YNewSmaller[i] <- sum(YNew[i,]==0) < sum(data$TA==0)
sum(YNewSmaller) / 15000


# Model validation
par(mar = c(5,5,2,2), cex.lab = 1.5, mfrow=c(2,2))
plot(x = data$DistReef, y = E, xlab = "DistReef", ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = data$HardCoral, y = E, xlab = "HardCoral", ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = data$LogSoakTime, y = E, xlab = "LogSoakTime", ylab = "Pearson residuals")
abline(h = 0, lty = 2)


# Model validation: spatial position --> equally suspicious as for the Poisson GLM
xyplot(Lat ~ Lon,
       col = 1,
       pch = 16,
       cex = 3 * sqrt(2 * abs(E) / max(abs(E))),
       data = data,
       xlab = list(label = "Longitude", cex = 1.5),
       ylab = list(label = "Latitude", cex = 1.5))


# ------------------------------------------------------------------------------
# number of silvertips sharks (STS):  Zero-Inflated Model (ZIP)
# ------------------------------------------------------------------------------
# Prepare data for JAGS:  c(count part) and b(binary part)
Xc <- model.matrix(~ 1 + DistReef.std + HardCoral.std + Zoning  + LogSoakTime.std, data = data)
Xb <- model.matrix(~ 1 + DistReef.std + HardCoral.std + Zoning  + LogSoakTime.std, data = data)
Kc <- ncol(Xc)
Kb <- ncol(Xb)
JAGS.data <- list(Y  = data$STS, # Response
                  Xc = Xc, # Covariates
                  Kc = Kc, # Number of betas
                  Xb = Xb, # Covariates
                  Kb = Kb, # Number of gammas
                  N  = nrow(data)
              )
JAGS.data


# Formulate JAGS modelling code
# JAGS code does not calculate expected values or Pearson residuals, and neigher does it simulate new data from the model,
# since ZIP models require a longer burn-in and more MCMC iterations to obtain good mixing.
sink("SharksGBRMP_ZIPGLM.txt")
cat("
    model{
    #1A. Priors regression parameters count part and binary part
    for (i in 1:Kc) { beta[i]  ~ dnorm(0, 0.0001) }  
    for (i in 1:Kb) { gamma[i] ~ dnorm(0, 0.0001) }  
    
    ###################################
    #2. Likelihood
    for (i in 1:N) {
    #Binary part. We need to work with 1 - Pi
    W[i] ~ dbern(1 - Pi[i])
    
    #Count process
    Y[i]       ~ dpois(W[i] * mu[i])
    
    #log-link and logistic link functions	
    log(mu[i])   <- inprod(beta[], Xc[i,]) 
    logit(Pi[i]) <- inprod(gamma[], Xb[i,])                                           
    }     
    }
    ",fill = TRUE)

sink()


# Create a vector with zeros and ones.
# This is ZIP specific stuff.
# Where ever we have non-zeros in STS, W = 1. 
W <- data$STS
W[data$STS > 0] <- 1


# Set initial values and parameters to save
inits  <- function () {
  list(beta       = rnorm(Kc, 0, 0.1),
       gamma      = rnorm(Kb, 0, 0.1),
       W          = W
  )  }

params <- c("beta", "gamma")


# Run JAGS
ZIP1   <- jags(data       = JAGS.data,
               inits      = inits,
               parameters = params,
               model      = "SharksGBRMP_ZIPGLM.txt",
               n.thin     = 10, 
               n.chains   = 3,
               n.burnin   = 4000,
               n.iter     = 5000)

# And do some updating
ZIP2  <- update(ZIP1, n.iter = 50000, n.thin = 10)  
ZIP3  <- update(ZIP2, n.iter = 50000, n.thin = 10)  
ZIP4  <- update(ZIP3, n.iter = 50000, n.thin = 10)  

outZIP <- ZIP4$BUGSoutput
print(outZIP)


# Assess mixing for count part
MyNames <- colnames(Xc)
MyBUGSChains(outZIP, uNames("beta", Kc), PanelNames = MyNames)


# Assess mixing for binary part
# Note that some chains have large negative values, which in the binary part correspoinds to pi = 0.
# May need to simplify the model
MyNames <- colnames(Xb)
MyBUGSChains(outZIP, uNames("gamma", Kb), PanelNames = MyNames)


# Present output for count part:  posterior means, standard errors and 95% credible interval
MyNames <- colnames(Xc)
OUT1 <- MyBUGSOutput(outZIP, uNames("beta", Kc), VarNames = MyNames)
print(OUT1, digits =4)
MyBUGSHist(outZIP, uNames("beta", Kc), PanelNames = MyNames)


# Present output for binary part:  posterior means, standard errors and 95% credible interval
MyNames <- colnames(Xb)
OUT1 <- MyBUGSOutput(outZIP, uNames("gamma", Kb), VarNames = MyNames)
print(OUT1, digits =4)
MyBUGSHist(outZIP, uNames("gamma", Kb), PanelNames = MyNames)


# Present results:  calculate fitted values and Pearson Residuals
beta  <- outZIP$sims.list$beta
gamma <- outZIP$sims.list$gamma
dim(beta)

mu <- exp(JAGS.data$Xc %*% t(beta))
Pi <- exp(JAGS.data$Xb %*% t(gamma)) / (1 +exp(JAGS.data$Xb %*% t(gamma)) )
ExpY <- (1 - Pi) * mu
varY <- (1 - Pi) * (mu + Pi * mu * mu)
E    <- (JAGS.data$Y - ExpY) / sqrt(varY)   
dim(E)

E.pm    <- rowSums(E, na.rm = TRUE) / ncol(E)
ExpY.pm <- rowSums(ExpY, na.rm = TRUE) / ncol(E)


# Model validation: posterior mean Pearson residuals versus posterior expected values
par(mfrow=c(1,1))
plot(x = ExpY.pm, y = E.pm)
abline(h = 0)     


# dispersion statistics
# --> 1.22  No serious overdispersion
p <- ncol(Xc) + ncol(Xb)
sum(E.pm^2) / (N - p)

