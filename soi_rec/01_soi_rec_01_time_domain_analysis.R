# setwd("//media//kswada//MyFiles//R//soi_rec")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
#   - Southern Oscillation Index (SOI), which measures changes in air pressure, related to sea surface temperatures in the central Pacific Ocean.
#     The central pacific warms every 3 to 7 years due to the El Nino effect, which has bee blamed for various global extreme weather events.
#
# data:  rec
#   - associated Recruitment (number of new fish)
# Both data are furnished by Dr. Roy Mendelssohn of the Pacific Environmental Fisheries Group (personal communication).
# Both series are for a period of 453 months ranging over the years 1950 - 1987.
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")




# ----------
is.ts(soi)

is.ts(rec)


# start, end, period
tsp(soi)

tsp(rec)



# ----------
head(soi, 120)

head(rec, 120)




# ------------------------------------------------------------------------------
# data exploration:  align time series and multivarite time series plot
# ------------------------------------------------------------------------------

# select intersect of time series and multivarite time series plot
( tmp <- ts.intersect(soi, rec) )



# ----------
graphics.off()

par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))

MTSplot(tmp)




# ----------
par(mfrow=c(2,1), mar = c(2,2,2,2))

plot(soi, ylab = "", xlab = "", main = "Southern Oscillation Index")
abline(v = seq(1950, 1990, by = 5), lty = 2, col = gray(0.4))

plot(rec, ylab = "", xlab = "", main = "Recruitment")
abline(v = seq(1950, 1990, by = 5), lty = 2, col = gray(0.4))




# -->
# The series show two basic oscillations types, an obvious annual cycle (hot in the summer, cold in the winter),
# and a slower frequency taht semmes th repeat about every 4 years.

# The two series are also related; it is easy to imagine the fish population is dependent on the ocean temperature.
# This possibility suggests trying some version of regression analysis as a procedure for relating the two series.
# Transfer function modelling can also be applied in this case.




# ------------------------------------------------------------------------------
# data exploration:  Smoothing
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(2,1))


plot(soi, type = "l")
lines(smooth.spline(soi, spar = 0.5), lwd = 2, col = "blue")
lines(smooth.spline(soi, spar = 1), lwd = 2, col = "red")


plot(rec, type = "l")
lines(smooth.spline(rec, spar = 0.5), lwd = 2, col = "blue")
lines(smooth.spline(rec, spar = 1), lwd = 2, col = "red")



# -->
# If the input series are nagated, the relationship between the series is positive.
# Strong similarity in the patterns of the tow series, and the slight lag of variations in the Output,
# behind those in the Input.




# ------------------------------------------------------------------------------
# forecast::ndiffs and differenced series
# ------------------------------------------------------------------------------

forecast::ndiffs(soi)

forecast::ndiffs(rec)




# ------------------------------------------------------------------------------
# Time series decomposision
# ------------------------------------------------------------------------------

plot(decompose(soi))


plot(decompose(rec))




# ------------------------------------------------------------------------------
# Month plot
# ------------------------------------------------------------------------------

monthplot(soi)


monthplot(rec)



# ------------------------------------------------------------------------------
# Correlation analysis:  Auto-correlation and partial-correlation
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

acf2(soi)


# -->
# 1 lag = 12 months  -->  0.5 lags = 6 months


acf2(diff(soi))



# ----------
acf2(rec)


acf2(diff(rec))




# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------

ccf(rec, soi, lag = 50)


# ----------
par(mfrow = c(1,1), mar = c(3,3,3,3))

acf(cbind(rec, soi), lag = 50)



# The dashed lines shown on the plots indicate +- 2 / sqrt(453) = +- 2 / sqrt(n);
# large sample distribution of cross-correlation is normal with mean zero and standard deviation = 1 / sqrt(n)
# if at least one of the processes is independent white noise.

# But since neither series is noise, these lines do not apply.

# in order for the dashed lines to be significant, at least one of the series must be white noise
# If this is not the case, there is no simple way to tell if a cross-correlation estimate is significantly different from zero
# We are only guessing at the linear dependence relationship between SOI and Recruitment



# ------------------------------------------------------------------------------
# lag plot:  real situation of cross-correlation
# ------------------------------------------------------------------------------

astsa::lag2.plot(soi, rec, max.lag = 12)

ccf(rec, soi, lag.max = 12, plot=FALSE)



# -->
# shows a fairly strong nonlinear relationship between Recruitment and SOI series, 




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
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



# ----------
# Examine inverse relation, namely, a regression model with the Recruitment series as input,
# implying a much simpler model

mod_rtos <- astsa::LagReg(input = rec, output = soi, L = 15, M = 32, inverse = TRUE,  threshold = 0.01)



# -->
# depending on only two coefficients
# x(t) = 0.41 + 0.016 * y(t+4) - 0.02 * y(t+5) + v(t)



# ----------
# Try rearranging

# By multiplying both sides by 50 * B^5  (B: backshift operator)
# 50 B^5 * x(t)         = 20.5 * B^5 + 0.8 * B^5 * y(t+4) - B^5 * y(t+5) + 50 * B^5 * v(t)
# 50 B^5 * x(t)         = 20.5 * B^5 + 0.8 * B   * y(t)   - y(t)         + 50 * B^5 * v(t)

# this model is converted to:
# (1 - 0.8 * B) * y(t) = 20.5 - 50 B^5 * x(t) + e(t)


# -->
# we apply
# for y:  lag -1
# for x:  lag -5

# This is parsimonious model



# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------


fish0 <- ts.intersect(R = rec, 
                     SL5 = stats::lag(soi, -5),
                     SL6 = stats::lag(soi, -6),
                     SL7 = stats::lag(soi, -7),
                     SL8 = stats::lag(soi, -8))

head(fish0)


( u0 <- lm(fish0[,1] ~ fish0[,2:5], na.action = NULL) )


summary(u0)



# ----------
# assess residuals

car::residualPlots(u0)


acf2(resid(u0))




# ----------
# incorporating ARIMA(2,0,X)

arx0 <- sarima(fish0[,1], 2, 0, 0, xreg = fish0[,2:5])

arx0 <- sarima(fish0[,1], 2, 0, 4, xreg = fish0[,2:5])


arx0$ttable





# ------------------------------------------------------------------------------
# Lagged regression again by parsimonious model
# ------------------------------------------------------------------------------

fish <- ts.intersect(R = rec, RL1 = stats::lag(rec, -1),  SL5 = stats::lag(soi, -5))


head(fish)


( u <- lm(fish[,1] ~ fish[,2:3], na.action = NULL) )



summary(u)



coef(u)



# -->
# note that the coefficients are corresponding to the implied parsimonious model
# (1 - 0.8 B) * y(t) = 20.5 - 50 * B^5 * x(t) + e(t)



# ----------
# assess residuals

car::residualPlots(u)


acf2(resid(u))



# -->
# note that residuals have ARIMA(p = 1, d = 0, q = 1)




# ----------
# incorporating ARIMA(1,0,1)

arx <- sarima(fish[,1], 1, 0, 1, xreg = fish[,2:3])

arx$ttable



# ----------
# adding MA terms
arx <- sarima(fish[,1], 1, 0, 6, xreg = fish[,2:3])

arx$ttable


arx$fit



# ----------
# output estimated by input

pred <- rec[-c(1:5)] - resid(arx$fit)

par(mfrow = c(1,1))

ts.plot(pred, rec[-c(1:5)], col = c('black', "gray"), lwd = c(2,2))



# ----------
arx0$AIC

arx$AIC



# -->
# slightly better for parsimonious model



# ----------
# mean squared residuals is smaller for parsimounious model

sum(resid(arx0$fit)[-c(1:8)]^2) / (453 - 8)

sum(resid(arx$fit)[-c(1:6)]^2) / (453 - 6)





# ------------------------------------------------------------------------------
# Transfer Function Model:  check which lag we should use ?
# ------------------------------------------------------------------------------


#############################
# Detrended SOI

soi.d <- resid(lm(soi ~ time(soi), na.action = NULL))


acf2(soi.d)



#############################
# pre-whitening of input

# check auto-correlation and partial-autocorrelation
acf2(soi.d)


# apply Ar(1) and take residuals
armod <- arima(soi.d, order = c(1, 0, 0), include.mean = FALSE)


soi.pw <- resid(armod)



# ----------
acf2(soi.pw, max.lag = 50)


# -->
# Still some irregular correlations are remained but short-term correlations are dissappeared.



#############################
# filter output by similar transformation AR(3)


# extract AR(1) coefficients
( ar1 <- as.numeric(coef(armod)[1]) )


ar1

armod$sigma2



# ----------
# filtering rec by AR(1)
# sides = 1:  filter coefficients are for past values only
# rec.fil:  the filtered (transformed) Recruitment series
# we apply the operator (1 - 0.588 * B) also to REC

rec.fil <- stats::filter(rec, filter = c(1, -ar1), sides = 1)



# ----------
( sa <- sqrt(armod$sigma2) )

( sb <- sd(rec.fil, na.rm = TRUE) )


sb / sa



# ----------
graphics.off()
par(mfrow = c(2,1))

ts.plot(cbind(soi, soi.pw), type = "l", col = c("gray", "black"))

ts.plot(cbind(rec, rec.fil), type = "l", col = c("gray", "black"))




#############################
# Check cross-correlation between output.fil vs. input.pw


par(mfrow = c(1,1))

tmp_ccf <- ccf(rec.fil, soi.pw, ylab = "CCF", na.action = na.omit, panel.first = grid())



# -->
# soi.pw leads to rec.fill by 5 lags



#############################
# impulse response function:  rec.fil vs. soi.pw

# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


mod_l2 <- astsa::LagReg(input = soi.pw[2:453], output = rec.fil[2:453], L = 15, M = 32, threshold = 2)


# -->
# clearly we can apply soi lag -5

