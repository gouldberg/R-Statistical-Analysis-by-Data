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
# lagged scatterplot matrices:  soi  vs.  rec
# Check the non-linear relationship between soi and rec
# ------------------------------------------------------------------------------

# Because we might wish to predict the Recruitment series from current or past values of the SOI series,
# it would be worthwhile to examine the scatterplot matrix.

astsa::lag2.plot(soi, rec, max.lag = 12)


Ccf(soi, rec, lag.max = 12, plot=FALSE)



# -->
# shows a fairly strong nonlinear relationship between Recruitment and SOI series, 
# the relationship is different between soi < 0 and soi > 0




# ------------------------------------------------------------------------------
# Regression with lagged variables 
# adding a dummy variable to account for the change of relationship
# ------------------------------------------------------------------------------

# Since we saw that the relationship is nonlinear and different when SOI is positive or negative.
# We may consider adding a dummy variable to account for this change.

dummy <- ifelse(soi < 0, 0, 1)

soiL6 <- stats::lag(soi, -6)

dL6 <- stats::lag(dummy, -6)


fish <- ts.intersect(rec, soiL6, dL6, dframe = TRUE)

fish




# ----------
# R(t) = beta0 + beta1 * S(t-6) + w(t)   if S(t-6) < 0
# R(t) = (beta0 + beta2) + (beta1 + beta3) * S(t-6) + w(t)   if S(t-6) >= 0

fit <- lm(rec ~ soiL6 * dL6, data = fish, na.action = NULL)

summary(fit)



# -->
# soiL6 : dL6 is significant




# ----------
par(mfrow = c(2,2))
plot(fit)



# plot(effects::allEffects(fit))






# ------------------------------------------------------------------------------
# Plot of Recruitment vs SOI lagged 6 months with the fitted values as points (+) and a lowess fit (-)
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(2,1))


# The piecewise regression fit is similar to the lowess fit
plot(fish$soiL6, fish$rec)

lines(lowess(fish$soiL6, fish$rec), col = 4, lwd = 2)

points(fish$soiL6, fitted(fit), pch = '+', col = 2)



# but note that residuals are not white noise
plot(resid(fit))



# ----------
acf2(resid(fit))


# -->
# The sample P(ACF) of the residuals indicates that an AR(2) model might be appropriate



# ------------------------------------------------------------------------------
# Fit AR(2) model and shows partial results
# ------------------------------------------------------------------------------

# sarima() fits ARIMA models (including improved diagnostics) in a short command.
mod_sarima <- sarima(rec, 2, 0, 0, xreg = cbind(soiL6, dL6, soiL6 * dL6))


mod_sarima$ttable




