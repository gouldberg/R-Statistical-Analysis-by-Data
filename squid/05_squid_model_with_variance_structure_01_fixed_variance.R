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
# The Fixed Variance Structure
#   - var(ei) = sigma^2 * DML(i)
#     larger residual spread if DML increases,
#     and the good news is that there are no extra parameters involved !
#   - The variance structure can be selected by specifying the weights arguments in the nlme::gls function
#   - Running the gls code without a weights option, gives you the same linear regression model.
#   - This model cannot be nested with linear regression model unless DML is equal to 1 for all observations.
# ------------------------------------------------------------------------------

library(nlme)


# In order to anova() comparison, we refit by gls without specifying weights argument.
M.lm <- gls(Testisweight ~ DML * fMONTH, data = Squid)



# ----------
# varFixed(~DML) ensures a variance that is proportional to DML.
vf1Fixed <- varFixed(~ DML)


M.gls1 <- gls(Testisweight ~ DML * fMONTH, weights = vf1Fixed, data = Squid)


anova(M.lm, M.gls1)



# -->
# The models are not nested, so no log-likelihood ratio test statistic is given, but the AIC clearly favours the model
# with the fixed variance.
# Note that both models have the same number of parameters !



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
E2 <- resid(M.gls1, type = "normalized")
coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")



# -->
# Note that "rstandard" does not applied to gls object ...



