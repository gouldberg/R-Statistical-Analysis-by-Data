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
# Fitting negative binomial GLM model in JAGS
# assess model, chain mixing
# ------------------------------------------------------------------------------

vars <- c("beta[1]", "beta[2]","beta[3]", "beta[4]", "size") 

MyBUGSChains(K2$BUGSoutput, vars)



# ------------------------------------------------------------------------------
# Fitting negative binomial GLM model in JAGS
# assess model, chain mixing
# ------------------------------------------------------------------------------

MyBUGSACF(K2$BUGSoutput, vars)



# -->
# There is minimal auto-correlation in the chains.
# If auto-correlation were present, we could ignore it, increase the thinning rate, simplify the model,
# or use a different method of sampling new iterations in teh MCMC process.



# ------------------------------------------------------------------------------
# Fitting negative binomial GLM model in JAGS
# Pearson residuals
# ------------------------------------------------------------------------------

OUT1 <- MyBUGSOutput(K2$BUGSoutput, vars) 

beta <- OUT1[1:4,1]

k <- OUT1[5,1]

eta <- win.data$X %*% beta

mu <- exp(eta)

EP <- (win.data$SqCones - mu) / sqrt(mu + mu^2 / k)  

par(mar = c(5,5,3,3))

plot(x = mu, 
     y = EP, 
     xlab = "Fitted values", 
     ylab = "Pearson residuals",
     cex.lab = 1.5)

abline(h = 0, lty = 2)



# ----------
SQ2$EP <- EP

MyVar <- c("Ntrees", "DBH", "TreeHeight", "CanopyCover")

Myxyplot(SQ2, MyVar, "EP")


# -->
# Here the model validation does not indicate problems
