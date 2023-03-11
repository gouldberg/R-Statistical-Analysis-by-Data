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
# Assess residuals
# ------------------------------------------------------------------------------

acf2(resid(u))



# -->
# the residuals have some autocorrelation
# residuals can be modeled as AR(1) model




# ------------------------------------------------------------------------------
# ARIMAX model
# ------------------------------------------------------------------------------

sarima(fish[,1], 1, 0, 0, xreg = fish[,2:3])




# -->
# Our final parsimonious fitted model is (with rounding)
# y(t) = 14.7 + 0.8 * y(t-1) - 21 * x(t-5) + e(t)
# e(t) = 0.45 * e(t-1) + w(t)

# where w(t) is white noise with sigma_w^2 = 49.57


# --> but not good fit ..
