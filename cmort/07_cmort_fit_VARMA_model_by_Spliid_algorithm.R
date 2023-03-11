setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# Fit VARMA model by Spliid algorithm by marima()
#   - Spliid algorithm uses the multivariate regression equations.
#     Experience suggests the estimators can be reasonably close to the maximum likelihood estimators.
#     The algorithm can be considered as a quick and easy way to fit an initial VARMA model as a starting point
#     to using maximum likelihood estimation, which is best done via state-space models
# ------------------------------------------------------------------------------

library(marima)


model <- define.model(kvar = 3, ar = c(1,2), ma = c(1))

arp <- model$ar.pattern

map <- model$ma.pattern



# ----------
# detrend cmort
cmort.d <- resid(detr <- lm(cmort ~ time(cmort), na.action = NULL))


# strip ts attributes
xdata <- matrix(cbind(cmort.d, tempr, part), ncol = 3)



# ----------
fit <- marima(xdata, ar.pattern = arp, ma.pattern = map, means = c(0, 1, 1), penalty = 1)


fit




# ------------------------------------------------------------------------------
# residual analysis
# ------------------------------------------------------------------------------

innov <- t(resid(fit))

plot.ts(innov)



# ----------
acf(innov, na.action = na.pass)




# ------------------------------------------------------------------------------
# fitted values for cmort
# ------------------------------------------------------------------------------

# one-step-ahead predictions

pred <- ts(t(fitted(fit))[,1], start = start(cmort), freq = frequency(cmort)) + detr$coef[1] + detr$coef[2] * time(cmort)


par(mfrow = c(1,1))

plot(pred, ylab = "Cardiovascular Mortality", lwd = 2, col = 4)


points(cmort)




# ----------
# print estimates and corresponding t^2-statistic

short.form(fit$ar.estimates, leading = FALSE)

short.form(fit$ar.fvalues, leading = FALSE)

short.form(fit$ma.estimates, leading = FALSE)

short.form(fit$ma.fvalues, leading = FALSE)




# ----------
# estimate of noise cov matrix

fit$resid.cov

