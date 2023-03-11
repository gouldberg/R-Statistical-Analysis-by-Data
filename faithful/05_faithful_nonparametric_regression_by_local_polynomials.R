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
# Non-parametric regresssion by local polynomials (loess smoothing)
#  - Both the kernel and spline methods have been relatively vulnerable to outliers.
#    The local polynomial method combines robustness ideas from linear regression and local fitting ideas from kernel methods.
#  - First we select a window, then fit a polynomial to teh data in that window using robust methods.
#    The predicted response at the middle of the window is the fitted value.
#    We then simply slide the window over the range of the data, repeating the fitting process as the window moves.
#    The most well-known implementation of this type of smoothing is called lowess or loess and is due to Cleveland (1979).
# ------------------------------------------------------------------------------

# default choice of the window width is three quarters of the data, but may not be good choice.

f <- loess(waiting ~ eruptions, faithful)
f

f_exa <- loess(y ~ x, exa)
f_exa

f_exa2 <- loess(y ~ x, exa, span = 0.22)
f_exa2

f_exb <- loess(y ~ x, exb)
f_exb

f_exb2 <- loess(y ~ x, exa, span = 1)
f_exb2


# ----------
i <- order(faithful$eruptions)

par(mfrow=c(1,3))

plot(waiting ~ eruptions, faithful, col = gray(0.75))
lines(f$fitted[i] ~ f$x[i], lty = 2)


plot(y ~ x, exa, col = gray(0.75))
lines(exa$x, exa$m, lty = 1)
lines(f_exa$fitted ~ f_exa$x, lty = 2)
lines(f_exa2$fitted ~ f_exa2$x, lty = 5)


plot(y ~ x, exb, col = gray(0.75))
lines(exb$x, exb$m, lty = 1)
lines(f_exb$fitted ~ f_exb$x, lty = 2)
lines(f_exb2$fitted ~ f_exb2$x, lty = 5)


# -->
# For the Old Faithful data, the default choice is satisfactory.
# For exa data, the default choice is too large. The choice that minimize the integrated squared error between the estimated and the true function 
# requires a span (proportion of the range) of 0.22.

# For exb data, the optimal choice of span is one (that is all the data).
# This is not surprising since the true function is a constant and so maximla smoothing is desired.
# We can see that the robust qualities of less prevent the fit from becoming too distorted by the two outliers even with the default choice of smoothing span.



# ------------------------------------------------------------------------------
# confidence bands
# ------------------------------------------------------------------------------

ggplot(exa, aes(x = x, y = y)) + geom_point(alpha = 0.25) + geom_smooth(method = "loess", span = 0.22) + geom_line(aes(x = x, y = m), linetype = 2)



# -->
# True function as a dashed line.
# We observe that the true function falls just outside the band in a few areas.
# However, the band we have constructed is a pointwise confidence band.
# The 95% confidence applies at each point but since we have a wide range of points,
# the 95% probability of the interval containing the true value cannot hold across the range.
# For  this we would need a simultaneous confidence band ...



# ----------
# band using splines: manually choosing the smoothing parameter, k = 20
library(mgcv)

ggplot(exa, aes(x = x, y = y)) + geom_point(alpha = 0.25) + geom_smooth(method = "gam", formula = y ~ s(x, k = 20)) + geom_line(aes(x = x, y = m), linetype = 2)

