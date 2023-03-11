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
# Regression with lagged variables:  impulse response function
#   - The high coherence between the SOI and Recruitment series suggests a lagged regression relation between the two series.
#     A natural direction for the implication in this situation is implied because we feel that the sea surface temperature of SOI
#     should be the input and the Recruitment series should be the output.
# ------------------------------------------------------------------------------


# estimated regression or impulse response function for SOI, with M = 32 and L = 15
# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero

mod_stor <- astsa::LagReg(input = soi, output = rec, L = 15, M = 32, threshold = 6)



# -->
# Note that the negative peak at a lag of five points;
# In this case, SOI is the input series.
# The fall-off after lag five seems to be approximately exponential and a possible model is
# y(t) = 66 - 18.5 * x(t-5) - 12.3 * x(t-6) - 8.5 * x(t-7) - 7 * x(t-8) + w(t)




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
# Examine inverse relation, namely, a regression model with the Recruitment series as input,
# implying a much simpler model
# ------------------------------------------------------------------------------

mod_rtos <- astsa::LagReg(input = rec, output = soi, L = 15, M = 32, inverse = TRUE,  threshold = 0.01)



# -->
# depending on only two coefficients
# x(t) = 0.41 + 0.16 * y(t+4) - 0.02 * y(t+5) + v(t)




# ------------------------------------------------------------------------------
# Try rearranging
# ------------------------------------------------------------------------------

# By multiplying both sides by 50 * B^5  (B: backshift operator),
# this model is converted to:
# (1 - 0.8 B) * y(t) = 20.5 - 50 B^5 * x(t) + e(t)


# -->
# we apply lag -1 for rec and -5 for soi

# This is parsimonious model of input = soi and output = rec





# ------------------------------------------------------------------------------
# Lagged regression again by parsimonious model
# ------------------------------------------------------------------------------

fish <- ts.intersect(R = rec, RL1 = stats::lag(rec, -1),  SL5 = stats::lag(soi, -5))


( u <- lm(fish[,1] ~ fish[,2:3], na.action = NULL) )



summary(u)



coef(u)



# -->
# note that the coefficients are corresponding to the implied parsimonious model
# (1 - 0.8 B) * y(t) = 20.5 - 50 * B^5 * x(t) + e(t)



