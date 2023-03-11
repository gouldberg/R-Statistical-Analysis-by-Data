setwd("//media//kswada//MyFiles//R//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------
Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


Squid$fMONTH = factor(Squid$MONTH)



# ------------------------------------------------------------------------------
# Compare models and choose optimal model
#   - In practice, it may be better to use the varPower, varExp, or varConstPower functions, which allow for more flexibility than the varFixed.
#   - Varioust variance structures
#        - varFixed:  Fixed variance
#        - varIdent:  Different variances per stratum
#        - varPower:  Power of the variance covariate
#        - varExp:    Exponential of the variance covariate
#        - varConstPower:  Constant plus power of the variance covariate
#        - varComb:   A combination of variance functions
# ------------------------------------------------------------------------------

anova(M.lm, M.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls6, M.gls7, M.gls8)


# -->
# The model allowing for an increase in spread for lorger DML values (which is allowed to differ per month), M.gls4, has the lowest AIC
# and is therefore selected as the optimal model.
# M.gls4:  varExp model an increase in spread for larger DML values, but only in certain months !
# Note that the tests above depend on the order in the anova command.



# ----------
# In the model of M.gls4, the interaction is highly significant.
anova(M.gls4)



# ------------------------------------------------------------------------------
# Graphical validation of the optimal model
# ------------------------------------------------------------------------------

E1 <- resid(M.gls4)

coplot(E1 ~ DML | fMONTH, ylab = "Ordinary residuals", data = Squid)


# -->
# Note that these residuals still show heterogeneity, but this is now allowed,
# because the residual variation differs depending on the chosen variance structure and values of the variance covariate.



# ----------
# You should use standardised residuals instead of the ordianry residuals for the model varidation.
# These are obtained by calculating the observed minus the fitted values and then dividing by the square root of the variance.
E2 <- resid(M.gls4, type = "normalized")

coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")


# -->
# There is no clear evidence of heterogeneity.


