# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)



# ----------
Squid$fMONTH = factor(Squid$MONTH)



# ------------------------------------------------------------------------------
# The VarIdent Variance Structure
#   - var(eij) = sigma(j)^2
#     Each month is allowed to have a different variance.
# ------------------------------------------------------------------------------

vf2 <- varIdent(form = ~ 1 | fMONTH)


M.gls2 <- gls(Testisweight ~ DML * fMONTH, weights = vf2, data = Squid)


anova(M.gls2)



# ----------
anova(M.lm, M.gls1, M.gls2)


# -->
# The AIC of the model using the different variances per month is lower.
# But notice that due to the variance structure, we now have to estimate 11 more parameters.



# ----------
anova(M.lm, M.gls2)


# -->
# null-hypothesis is H0: sigma1^2 = sigma2^2 = ... = sigma12^2
# The log likelihood ratio test indicates that the model with different variances per month is better, allowing us to reject the null hypothesis
# that all variances are the same.



# ----------
# This gives the different varianc estimation
summary(M.gls2)


# The numbers under the months (2, 9, 12, etc.) are multiplication factors.
# They show the ratio with the estimated residual standard error (1.27), the estimator for sigma.
# In month 9, the variance is 2.99 * residual standard error (1.27).
# Note that months 9 and 10, and 3 have the highest ratios indicating that in these months there is more residual variation.



# ------------------------------------------------------------------------------
# plot observations and conditional plot of the residuals versus DLM
# ------------------------------------------------------------------------------

# colours observations of the same month.
plot(M.lm, which = c(1), col = Squid$MONTH, add.smooth = FALSE, caption = "")


# -->
# The graph does not show any clear grouping.



# ----------
# coplot of the residuals versus DLM, conditional on month
# the lower left panel corresponds to month 1, the lower right to month 4, and the upper right to month 12.
# You should use standardised residuals instead of the ordianry residuals for the model varidation.
# These are obtained by calculating the observed minus the fitted values and then dividing by the square root of the variance.
library(lattice)
E2 <- resid(M.gls2, type = "normalized")
coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")



# -->
# Note that the normalized variance at month 9 and 10 is still large
# Also normalized residuals are not constant against DML


