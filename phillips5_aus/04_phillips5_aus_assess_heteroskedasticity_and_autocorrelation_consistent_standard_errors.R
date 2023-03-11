# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/phillips5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  phillips5_aus
# ------------------------------------------------------------------------------
data("phillips5_aus", package = "POE5Rdata")

data <- phillips5_aus

glimpse(data)

str(data)

dim(data)


# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,3,4)], start = c(1987, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# OLS estimate of the Phillips curve equation
#   - inf(t) = alpha + beta0 + D(u(t)) + e(t)
# ------------------------------------------------------------------------------
phill.dyn <- dynlm(inf ~ diff(u), data = data.ts)


kable(tidy(phill.dyn), digits=3, caption = "Summary of the Phillips Model")



# ------------------------------------------------------------------------------
# Model assessment:  Comparing 3 version of HAC standard errors for the Phillips Curve model
#   - Similar to the case of heteroskedasticity, autocorrelation in the errors does not produce biased estimates of the coefficients in linear regression,
#     but it produces incorrect standard errors.
#     With autocorrelation, it is possible to calculate correct (HAC: heteroskedasticity and autocorrelation consistent) standard erros, known as Newey-West standard errors.
# ------------------------------------------------------------------------------
library(sandwich)
ac <- acf(residuals(phill.dyn), plot=FALSE)
( s0 <- coeftest(phill.dyn) )
( s1 <- coeftest(phill.dyn, vcov. = vcovHAC(phill.dyn)) )
( s2 <- coeftest(phill.dyn, vcov. = NeweyWest(phill.dyn)) )
( s3 <- coeftest(phill.dyn, vcov. = kernHAC(phill.dyn)) )

tbl <- data.frame(cbind(s0[c(3,4)], s1[c(3,4)], s2[c(3,4)], s3[c(3,4)]))
names(tbl) <- c("Incorrect", "vcovHAC", "NeweyWest", "kernHAC")
row.names(tbl) <- c("(Intercept", "Du")
kable(tbl, digits = 3, caption = "Comparing Standard Errors for the Phillips model")



# -->
# Correcting the standard erros in a model with autocorrelated errors does not make the estimator of teh coefficients a minimum-variance one.
# We need to find better estimators.WW

