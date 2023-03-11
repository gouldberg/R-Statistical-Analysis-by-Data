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
# Model Poisson GLM:  model validation
# compute dispersion parameters
# ------------------------------------------------------------------------------

E1  <- resid(M1, type = "pearson")

sum(E1^2) / (nrow(SQ2) - length(coef(M1)))


# -->
# The dispersion parameter is considerable reater than 1: hence there is overdispersion.



# ------------------------------------------------------------------------------
# Model Poisson GLM:  model validation
# Pearson residuals and Cook's distance
# Cook's distance values is a leave-one-observation-out measure of influence.
# ------------------------------------------------------------------------------

par(mfrow = c(2,2), mar = c(5,5,2,2))

plot(M1)



# ----------
F1 <- fitted(M1, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))

plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Pearson residuals", cex.lab = 1.5)
abline(h=0, lty = 2)

plot(cooks.distance(M1), type = "h", xlab = "Observation", ylab = "Cook distance", cex.lab =  1.5)

plot(y = SQ2$SqCones, x = F1,
     xlab = "Fitted values",
     ylab = "Observed data",
     cex.lab = 1.5,xlim = c(0,60), ylim = c(0,60) )

abline(coef=c(0, 1), lty = 2)   



# -->
# Note that there are some observations with large Pearson residuals.
# There are at least 4 observations that have a large influence on the fitted values



# ------------------------------------------------------------------------------
# Model Poisson GLM:  model validation
# Plotting Pearson residuals versus each covariate in the model
# ------------------------------------------------------------------------------

MyVar <- c("Ntrees.std", "DBH", "TreeHeight.std", "CanopyCover.std")

SQ2$E1 <- E1    

Myxyplot(SQ2, MyVar, "E1")


# -->
# But we find no clear non-linear patterns to warrant the use of, for example, generalized additive modelling ....



# ------------------------------------------------------------------------------
# Model Poisson GLM:  model validation
# Check zero values
# ------------------------------------------------------------------------------

sum(SQ2$SqCone==0) / nrow(SQ2)  


# -->
# Only 10% of the dat a equals zero, hence the overdispersion is not caused by zero inflation.
# If the overdispersion in a Poisson GLM is due to large variation, the negative binomial GLM is a candidate model.





# The Clevelend dotplot for SqCones 
