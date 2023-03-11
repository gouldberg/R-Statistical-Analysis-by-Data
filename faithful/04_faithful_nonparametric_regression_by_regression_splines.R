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
# Non-parametric regresssion by regression splines  (basis function approach)
#
#  - For regression splines, the knots of the B-splines used for the basis are typically much smaller in number than the sample size.
#    The number of knots chosen controls the amount of smoothing
#  - For smoothing splines, the observed unique x values are the knots and lambda is used to control the smoothing.
#  - It is arguable whether the regression spline method is parametric or nonparametric, because once the knots are chosen, a parametric family
#    has been specified with a finite number of parameters.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Non-parametric regresssion by regression splines with adjusting the knots
# ------------------------------------------------------------------------------

rhs <- function(x, c) ifelse(x > c, x - c, 0)


# one basis function for linear regression splines
par(mfrow=c(1,1))
curve(rhs(x, 0.5), 0, 1)



# ----------
knots <- 0:9/10
knots


# compute design matrix of splines with knots at these points for each x
dm <- outer(exa$x, knots, rhs)
dm

matplot(exa$x, dm, type = "l", col = 1)



# ----------
# evenly spaced knots fit
g <- lm(exa$y ~ dm)
g

plot(y ~ x, exa, col = gray(0.75), xlab = "x", ylab = "y")
lines(exa$x, predict(g))


# -->
# Because the basis functions are piecewise linear, the fit is also piecewise linear.


# ----------
# A better fit may be obtained by adjusting the knots so that they are denser in regions of greater curvature.
# knots spread relative to the curvature
newknots <- c(0, 0.5, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
dmn <- outer(exa$x, newknots, rhs)

gn <- lm(exa$y ~ dmn)
plot(y ~ x, exa, col = gray(0.75), xlab = "x", ylab = "y")
lines(exa$x, predict(gn))



# ------------------------------------------------------------------------------
# Non-parametric regresssion by regression splines with smoother fit by using higher-order splines
# ------------------------------------------------------------------------------

library(splines)

# bs() can be used to generate the appropriate spline basis
# the default is cubic B-splines
bsp <- bs(seq(0, 1, length = 1000), df = 12)
bsp

matplot(bsp, type = "l", ylab = "", col = 1)



# ----------
sml <- lm(waiting ~ bs(eruptions, 12), faithful)
sml

sml_exa <- lm(y ~ bs(x, 12), exa)
sml_exa

sml_exb <- lm(y ~ bs(x, 12), exb)
sml_exb



# ----------
par(mfrow=c(1,3))

plot(waiting ~ eruptions, faithful, col = gray(0.75))
i <- order(faithful$eruptions)
lines(predict(sml)[i] ~ eruptions[i], faithful, lty = 2)

plot(y ~ x, exa, col = gray(0.75))
lines(exa$x, exa$m)
lines(predict(sml_exa) ~ x, exa, lty = 2)

plot(y ~ x, exb, col = gray(0.75))
lines(exb$x, exb$m)
lines(predict(sml_exb) ~ x, exb, lty = 2)

