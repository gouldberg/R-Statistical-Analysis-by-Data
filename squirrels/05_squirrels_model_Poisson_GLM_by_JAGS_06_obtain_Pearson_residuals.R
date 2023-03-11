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
# Model validation:  Obtain Pearson residuals
# Option 1:  Calculate Pearson residuals within MCMC algorithm
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
plot(K2$BUGSoutput$mean$PRes)



# -->
# The problem with this approach is that, for large datasets and large numbers of MCMC iterations,
# we may encounter memory problems, as the residual for each observation i comes with a large number of MCMC iterations.



# ------------------------------------------------------------------------------
# Model validation:  Obtain Pearson residuals
# Option 2:  Calculate posteriror distributions of the Pearson residuals retrospectively
# ------------------------------------------------------------------------------

beta <- K2$BUGSoutput$sims.list$beta

dim(beta)


mu <- exp(win.data$X %*% t(beta))

PRes <- (win.data$SqCones - mu) / sqrt(mu)

dim(PRes)


# Posterior mean of each Pearson residual
PRes.pm <- rowSums(PRes) / 3000


# Posterior mean of fitted values
mu.pm <- rowSums(mu) / 3000



# ----------
graphics.off()
par(mfrow=c(1,2))
plot(PRes.pm, main = "Posterior mean of Pearon residuals")
plot(PRes.pm ~ mu.pm, main = "Peason Residuals vs. Fittted")



# ------------------------------------------------------------------------------
# Model validation:  Obtain Pearson residuals
# Option 3:  Calculate posteriror mena of each regression parameter and use these to calculate Pearson residuals
# ------------------------------------------------------------------------------

beta.pm <- K2$BUGSoutput$mean$beta

length(beta.pm)


mu  <- exp(eta <- win.data$X %*% beta.pm)      

PRes <- (win.data$SqCones - mu) / sqrt(mu)

length(PRes)



# ----------
graphics.off()
par(mfrow=c(1,2))
plot(PRes, main = "Posterior mean of Pearon residuals")
plot(PRes ~ mu, main = "Peason Residuals vs. Fittted")


