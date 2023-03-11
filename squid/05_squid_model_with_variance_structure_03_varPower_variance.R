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
# The varPower Variance Structure
#   - var(eij) = sigma(j)^2 * | DML(ij) |^(2*delta)
#     The variance of the residuals is modelled as simga^2, multiplied with the power of the absolute value of the variance covariate DML.
#   - If delta = 0, we obtain the linear regression model, meaning this model and linear regression model are nested,
#     and therefore the likelihood ratio test can be applied to judge which one is better.
#   - If delta = 2 and a variance covariate with positive values, we get the same variance structure as "Fixed Variance Structure".
#   - If the variance covariate has values equal to zero, the variance of the residuals is zero as well.
#     This causes problems in the numerical estimation process, and if the variance covariate has values equal to zero, the varPower should not be used.
#     For the squid data, all DML values are larger than 0 (DML is length); so it is not a problem with this example.
# ------------------------------------------------------------------------------

vf3 <- varPower(form = ~ DML)

M.gls3 <- gls(Testisweight ~ DML * fMONTH, weights = vf3, data = Squid)

summary(M.gls3)



# -->
# The AIC of this model, which is the lowest value so far.
# delta = 1.76



# ----------
# model an increae in spread for larger DML values, but only in certain months !
vf4 <- varPower(form = ~ DML | fMONTH)

M.gls4 <- gls(Testisweight ~ DML * fMONTH, data = Squid, weights = vf4)

summary(M.gls4)



# -->
# Instead of having one delta, we now have 12 of them. There is little variation between the estimated values of delta, but keep in mind they are 
# multiplid by two, before being used to take the power.
# It is also possible to set the delta for some months equal to an a priori chosen value and keep it fixed.
# This is handy if you know or wnat to test whether the spread along DML in some months is constant.
# This can be done with teh fixed option in varPower.



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
E2 <- resid(M.gls3, type = "normalized")
coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")


E2 <- resid(M.gls4, type = "normalized")
coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")
