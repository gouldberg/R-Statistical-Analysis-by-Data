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
# Correlation analysis:  check by fitting autoregressive model to Recruitment series
# ------------------------------------------------------------------------------

# Fit an autoregressive time series model to the data by ordinary least squares, by default selecting the complexity by AIC
# x(t) = phi0 + phi1 * x(t-1) + phi2 * x(t-2) + w(t):  for t = 3, 4, ..., 453

( regr <- ar.ols(rec, order = 2, demean = FALSE, intercept = TRUE) )


regr


# -->
# phi0 = 6.74 (intercept),  phi1 = 1.35,  phi2 = -0.46,  and sigma^2 = 89.72


# standard errors of the estimates
regr$asy.se.coef

