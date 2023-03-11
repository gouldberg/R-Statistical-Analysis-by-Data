# ------------------------------------------------------------------------------
# Data: Beavers
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "lattice", "ggplot2", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.csv(file = "./ZuurZero-InflatedModelswithR/Beavers2.csv", header=T)
dim(data)
str(data)

data$MD <- data$Mid_line_Distance
data$PD <- data$Petiole_Diameter

Hmisc::describe(data)

is.na(data)
colSums(is.na(data))

dim(data)
data2 <- data[!is.na(data$PD),]
dim(data2)


# Note that we do not have data for 3 lochs for 2012
table(data$Loch, data$Year)


# ------------------------------------------------------------------------------
# Standardize for MCMC mixing
# ------------------------------------------------------------------------------
data2$PD.std <- (data2$PD - mean(data2$PD)) / sd(data2$PD)


# ------------------------------------------------------------------------------
# Missing Values
# ------------------------------------------------------------------------------
is.na(data2)
colSums(is.na(data2))

# We still have 47 observations with missing vaues for the response variable MD. and they all occurred at lochs were beavers were present 
table(is.na(data2$MD), data2$Beaver)

# We have 64 observations where beavers were present, but 47 of these 64 observations did not have a midline diameter value
table(data2$Loch, is.na(data2$MD), data2$Beaver)


# ------------------------------------------------------------------------------
# Random Intercept model by JAGS
# ------------------------------------------------------------------------------
X <- model.matrix(~ PD.std, data = data2)
K <- ncol(X)  #Number of betas

# Random effects
# We need a vector with values 1 1 1 2 2 2 3 3 4 ...
Loch <- as.numeric(as.factor(data2$Loch))
Nre <- length(unique(Loch))

JAGS.data <- list(Y   = data2$MD,    # Response
                  X    = X,         # Covariate
                  K    = K,         # Number of betas
                  N    = nrow(data2), # Sample size
                  Loch = Loch,      # Random effect ID
                  Nre  = Nre)       # Number of random effects
JAGS.data


# JAGS modelling code
sink("Beavers2_JAGS_mixedmodel.txt")
cat("
    model{
    #1A. Diffuse normal priors beta 
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}
    
    #1B. Diffuse uniform prior sigma
    sigma ~ dunif(0, 20)
    tau  <- 1 / (sigma * sigma) 
    
    #1C. Priors random intercepts and random slopes
    for (i in 1:Nre) { a[i] ~ dnorm(0, tau_Loch) }
    
    #1D. Diffuse uniform prior for sigma_Site
    #    This one is for the random intercept
    tau_Loch  <- 1 / (sigma_Loch * sigma_Loch)
    sigma_Loch ~ dunif(0, 100)
    
    #2. Likelihood (with specific coding)
    for (i in 1:N) {
    Y[i]    ~ dnorm(mu[i], tau)
    eta[i] <- inprod(beta[], X[i,]) #Covariates
    mu[i]  <- eta[i]  + a[Loch[i]]  #Covariates + random intercepts 
    }
    
    #Residuals    
    for (i in 1:N) {
    Res[i]    <- Y[i] - mu[i] 
    }
    }
    ",fill = TRUE)
sink()


# Initial values & parameters to save
inits  <- function () {
  list(
    beta       = rnorm(K,  0, 0.1),  # betas
    sigma      = runif(1,  0, 20),   # sigma eps
    a          = rnorm(Nre, 0, 0.1),  # random intercepts
    sigma_Loch = runif(1,  0, 100))  } # sigma random intercepts

params <- c("beta", "sigma", "sigma_Loch", "Res", "a",  "mu", "Y")


# Start JAGS
G1 <- jags(data       = JAGS.data,
           inits      = inits,
           parameters = params,
           model      = "Beavers2_JAGS_mixedmodel.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

G2   <- update(G1, n.iter = 10000, n.thin = 10)
outG <- G2$BUGSoutput


# ------------------------------------------------------------------------------
# Random Intercept model by JAGS
# Asess mixing and present numerical output / posterior information
# ------------------------------------------------------------------------------
source(file = ".//ZuurZero-InflatedModelswithR//MCMCSupportHighstatV4.R")
source(file = ".//ZuurZero-InflatedModelswithR//HighstatLibV10.R")


# Assess mixing and auto-correlation
# Adjust this variable if extra parameters (e.g an ICC) are added!
MyNames <- c("Intercept", "PD",  "sigma eps", "sigma Loch")
MyBUGSChains(outG,  c(uNames("beta", K), "sigma", "sigma_Loch"), PanelNames = MyNames)


# Present numerical output and posterior distribution
OUT3 <- MyBUGSOutput(outG, c(uNames("beta", K), "sigma", "sigma_Loch"), VarNames = MyNames)
print(OUT3, digits = 5)


MyBUGSHist(outG, c(uNames("beta", K), "sigma", "sigma1_Site"), PanelNames = MyNames)



# ------------------------------------------------------------------------------
# Random Intercept model by JAGS
# Model validation
# We can use posterior mean residuals and posterior fitted values for model validation
# ------------------------------------------------------------------------------
E1.mcmc <- outG$mean$Res  #Posterior mean of the residuals
F1.mcmc <- outG$mean$mu   #This is with random effects


# Residuals versus fitted value
par(mfrow = c(1,1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = F1.mcmc, y = E1.mcmc, xlab = "Posterior mean values", ylab = "Posterior mean residuals")
abline(h = 0, lty = 2)


# Residuals versus Covariate
par(mfrow = c(1,1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = data2$PD, y = E1.mcmc, xlab = "PD", ylab = "Posterior mean residuals")
abline(h = 0, lty = 2)


# Residuals by Beaver absence/presence
par(mfrow = c(1,1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
boxplot(E1.mcmc ~ Beaver,  data = data2, ylab = "Posterior mean residuals", xlab = "Beaver absence/presence", varwidth = TRUE)
abline(h = 0, lty = 2)


# Residuals by Loch --> There eare no clear violations, but a small amount of heterogeneity ?
par(mfrow = c(1,1), mar = c(5, 5, 2, 2), cex.lab = 1.5)
boxplot(E1.mcmc ~ Loch, data = data2, ylab = "Posterior mean residuals", xlab = "Loch", varwidth = TRUE)
abline(h = 0, lty = 2)



# ------------------------------------------------------------------------------
# Random Intercept model by JAGS
# Visualizing the results
# ------------------------------------------------------------------------------
MyData <- expand.grid(PD.std = seq(min(data2$PD.std), max(data2$PD.std), length = 25))
Xp     <- model.matrix(~ PD.std , data = MyData)
dim(Xp)


# We have a large number of MCMC values of the betas.
# We can calculate the fitted values for each MCMC iteration  
# and use these to calculate credible intervals:
# Get the MCMC betas
Beta.mcmc <- outG$sims.list$beta 
dim(Beta.mcmc)


# Calculate the fitted values for each MCMC iteration
# These are fitted values for a typical site 
# (typical = average = 0)
mu.mcmc <- Xp %*% t(Beta.mcmc)  #25 by 4 times 4 by 3000   
dim(mu.mcmc)

# Note that this mu.mcmc contains predicted values
# for the 25 artifical covariate values.


# Why don't we take the 2.5% and 97.5% values at each of the artificial covariate values?
# And plot these instead of the 3,000 lines? 

par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = 0, y = 0, xlab = "Standardized PD", ylab = "Fitted MD values", type = "n", xlim = c(-2.5, 2.5), ylim = c(0, 225))	
for (i in 1:100) lines(x = MyData$PD.std, y = mu.mcmc[,i])


# GetCIs(): Support file to calculate the posterior mean, se and 2.5 and 97.5 quantiles for fitted values. The input needs to be an observation - by - MCMC iteration object.
L <- GetCIs(mu.mcmc)
head(L)
MyData2 <- cbind(MyData, L)
MyData2

# To back-standardize
MyData2$PD <- MyData$PD.std * sd(data2$PD) + mean(data2$PD)


p <- ggplot()
p <- p + geom_point(data = data2, aes(y = MD, x = PD), shape = 1,  size = 1)
p <- p + xlab("Petiole diameter (mm)") + ylab("Midline diameter (mm)")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_line(data = MyData2,  aes(x = PD, y = mean),  colour = "black")
p <- p + geom_ribbon(data = MyData2,  aes(x = PD,  ymax = up,  ymin = lo), alpha = 0.4)
p



# ------------------------------------------------------------------------------
# Random Intercept model by JAGS
# Missing Values
# ------------------------------------------------------------------------------
cbind(outG$mean$Y, JAGS.data$Y) %>% head(10)

boxplot(outG$mean$Y ~ Beaver, data = data2, xlab = "Beaver absence / presence", ylab = "Observed and predicted MD values")


I     <- is.na(data2$MD)
N     <- nrow(data2)
Index <- 1:N
Index.NAs <- Index[I]
MD.mcmc <- outG$sims.list$Y[,Index.NAs]
dim(MD.mcmc)
PD.na <- data2[Index.NAs, "PD"]

L <- GetCIs(t(MD.mcmc))
MyData3 <- data.frame(L, PD.na)
MyData3

p <- p + geom_point(data = MyData3,  aes(y = mean, x = PD.na), shape = 16,  size = 4)
p <- p + geom_errorbar(data = MyData3, aes(x = PD.na, ymax = up, ymin = lo),  width = 0.2)                     
p


I <- data2$PD > 5 
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
boxplot(outG$mean$Y[I] ~ Beaver[I],  data = data2, xlab = "Beaver absence / presence", ylab = "Observed and predicted MD values", varwidth = TRUE)



