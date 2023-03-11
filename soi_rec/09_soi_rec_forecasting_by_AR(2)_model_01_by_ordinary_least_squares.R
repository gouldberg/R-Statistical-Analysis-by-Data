setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data:  rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")



# ------------------------------------------------------------------------------
# Forecasting the Recruitment series by AR(2) model by ordinary least squares
# ------------------------------------------------------------------------------

# Fit an autoregressive time series model to the data by ordinary least squares, by default selecting the complexity by AIC
# x(t) = phi0 + phi1 * x(t-1) + phi2 * x(t-2) + w(t):  for t = 3, 4, ..., 453

( regr <- ar.ols(rec, order = 2, demean = FALSE, intercept = TRUE) )


regr


# -->
# phi0 = 6.74 (intercept),  phi1 = 1.35,  phi2 = -0.46,  and sigma^2 = 89.72


# standard errors of the estimates
regr$asy.se.coef



# ----------
fore <- predict(regr, n.ahead = 24)


fore



# ----------
par(mfrow = c(1,1))
ts.plot(rec, fore$pred, col = 1:2, xlim = c(1980, 1990), ylab = "Recruitment")


U <- fore$pred + fore$se
L <- fore$pred - fore$se
xx <- c(time(U), rev(time(U)))
yy <- c(L, rev(U))


polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.2))
lines(fore$pred, type = "p", col = 2)


# -->
# Note how the forecast levels off quickly and the prediction intervals are wide,
# even though in this case the forecast limits are only based on one standard error


