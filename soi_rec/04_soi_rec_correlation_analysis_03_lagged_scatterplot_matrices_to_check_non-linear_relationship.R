setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data; rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")




# ------------------------------------------------------------------------------
# Correlation analysis:  lagged scatterplot matrices
#   - to check for nonlinear relations
# ------------------------------------------------------------------------------

graphics.off()

astsa::lag1.plot(soi, max.lag = 12)

Acf(soi, lag.max = 12, plot=FALSE)



# -->
# The sample autocorrelations are displayed in the upper right-hand corner and superimposed on the scatterplots are locally weighted scatterplot
# smoothing (lowess) lines can be used to help discover any nonlinearities.

# We notice that the lowess fits are approximately linear, so that the sample autocorrelations are meaningful
# We see strong positive linear relations at lags = 1, 2, 11, 12, and negative linear relation at lags h = 6, 7
# These results match up well with peak noticed in ACF plot



# ----------
# more lags --> 12

astsa::lag1.plot(rec, max.lag = 24)

Acf(rec, lag.max = 24, plot=FALSE)




# ------------------------------------------------------------------------------
# Correlation analysis:  lagged scatterplot matrices:  soi vs rec
# ------------------------------------------------------------------------------

# Because we might wish to predict the Recruitment series from current or past values of the SOI series,
# it would be worthwhile to examine the scatterplot matrix.

astsa::lag2.plot(soi, rec, max.lag = 12)

Ccf(soi, rec, lag.max = 12, plot=FALSE)



# -->
# shows a fairly strong nonlinear relationship between Recruitment and SOI series, 
# indicating the SOI series tends to lead the Recruitment series
# and the coefficients are negative, implying that increases in the SOI lead to decrease in the Recruitment.

