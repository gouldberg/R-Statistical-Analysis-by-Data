setwd("//media//kswada//MyFiles//R//cairo")

packages <- c("dplyr", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cairo
# ------------------------------------------------------------------------------

data("cairo", package = "gamair")

str(cairo)

head(cairo)



# ----------
car::some(cairo)



# ------------------------------------------------------------------------------
# model with corAR1 and cyclic smoothers
# ------------------------------------------------------------------------------


# cc: cyclic function
# The data cover some leap years, where day.of.year runs from 1 to 366.
# This means that by defaul the cycle smooth matches at at day 1 and 366, which is correct in non-leap years, but not quite right in the leap years themselves.
# Nested the AR model for the residuals within year:  this vastly speeds up computation, but is somewhat arbitrary.

ctamm <- gamm(temp ~ s(day.of.year, bs = "cc", k = 20) + s(time, bs = "cr"), data = cairo, correlation = corAR1(form = ~1 | year))



# ----------
summary(ctamm$gam)



# -->
# It seems that there is evidence for a long term trend in temperature, and that the model fits fairly closely.



# ------------------------------------------------------------------------------
# 95% confidence intervals for the variance parameter and correlation structure
# ------------------------------------------------------------------------------

intervals(ctamm$lme, which = "var-cov")



# -->
# Very strong evidence that the AR1 model is preferable to an independence model (phi = 0), while the interval for sigma is (3.92, 4.23)



# ------------------------------------------------------------------------------
# Parametrization
# ------------------------------------------------------------------------------

# The parameterization is not completely obvious:  the reported parameters are sigma^2 / lambda
ctamm$gam$sig2 / ctamm$gam$sp



# ------------------------------------------------------------------------------
# Estimated model terms
# ------------------------------------------------------------------------------

plot(ctamm$gam, scale = 0)

plot(ctamm$gam, scale = 1)


# -->
# The temperature increase appears to be quite marked !!
