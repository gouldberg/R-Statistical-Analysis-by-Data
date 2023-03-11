setwd("//media//kswada//MyFiles//R//faithful")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  faithful
# ------------------------------------------------------------------------------

data(faithful, package="faraway")

str(faithful)


# ----------
# for comparison
# exa:  true function is f(x) = sin^3(e * pi * x^3)
# exb:  constant zero

data(exa);  data(exb)

str(exa)

str(exb)



# ------------------------------------------------------------------------------
# plot data
# ------------------------------------------------------------------------------

par(mfrow=c(1,3))

plot(y ~ x, exa, main = "Example A", pch = ".")
lines(m ~ x, exa)

plot(y ~ x, exb, main = "Example B", pch = ".")
lines(m ~ x, exb)

plot(waiting ~ eruptions, faithful, main = "Old Faithful", pch = ".")



# ------------------------------------------------------------------------------
# Non-parametric regresssion by smoothing splines
#
#  - we choose function to minimize a modfied least squares criterion
#     = MSE + lambda * roughness penalty
#       roughness penalty = integral([f''(x)]^2 * dx)
#  - if f is a cubic spline, this means that f is a piecewise cubic polynomial in each interval, and f, f' and f'' are continuous.
#
#  - Robust version of criterion
#     = sum(rho * (yi - f(xi)) + lambda * roughness penalty,  where rho = |x|
# ------------------------------------------------------------------------------

# cross-validation is used to select the smoothing parameter by default.

ssp <- smooth.spline(faithful$eruptions, faithful$waiting)
ssp

ssp_exa <- smooth.spline(exa$x, exa$y)
ssp_exa

ssp_exb <- smooth.spline(exb$x, exb$y)
ssp_exb



# ----------
par(mfrow=c(1,3))

plot(waiting ~ eruptions, faithful, pch = ".")
lines(ssp)

plot(y ~ x, exa, pch = ".")
lines(exa$x, exa$m)
lines(ssp_exa, lty = 2)
      
plot(y ~ x, exb, pch = ".")
lines(exb$x, exb$m)
lines(ssp_exb, lty = 2)


# -->
# The fit for exa dos a good job of tracking the hills and valleys but overfits in the smoother region.
# The default choice of smoothing parameter given by CV is a disaster for exb as the data is just interpolated.
# This illustrates the danger of blndly relying on automatic bandwidth selection methods.


