setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\gasfurnace")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
#   - Series of input and output measurements on a gas furnace that have been previously modeled by spectral analysis
#     in Jenkins and Watts (1968), and by lagged correlation methods in Box and Jenkins (1970).
#   - Variable:
#       - input:   actual gas rate in cubic feet per minute.
#       - output:  the concentration of carbon dioxide measured as a percentage of the outlet gas from the furnace.
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)





# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

MTS::MTSplot(gasf)





# ------------------------------------------------------------------------------
# data exploration:  Smoothing
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(2,1))


plot(gasf$input, type = "l")
abline(v = seq(0, 300, by = 25), lty = 2, col = "gray")
lines(smooth.spline(time(gasf$input), gasf$input, spar = 0.5), lwd = 2, col = 4)
lines(smooth.spline(time(gasf$input), gasf$input, spar = 1), lwd = 2, col = 4)



plot(gasf$output, type = "l")
abline(v = seq(0, 300, by = 25), lty = 2, col = "gray")
lines(smooth.spline(time(gasf$output), gasf$output, spar = 0.5), lwd = 2, col = 4)
lines(smooth.spline(time(gasf$output), gasf$output, spar = 1), lwd = 2, col = 4)




# -->
# If the input series are nagated, the relationship between the series is positive.
# Strong similarity in the patterns of the tow series, and the slight lag of variations in the Output,
# behind those in the Input.




# ------------------------------------------------------------------------------
# forecast::ndiffs and differenced series
# ------------------------------------------------------------------------------

apply(gasf, 2, forecast::ndiffs)



par(mfrow=c(2,1))


plot(diff(gasf$input), type = "l")
lines(smooth.spline(time(diff(gasf$input)), diff(gasf$input), spar = 0.5), lwd = 2, col = 4)
lines(smooth.spline(time(diff(gasf$input)), diff(gasf$input), spar = 1), lwd = 2, col = 4)

plot(diff(gasf$output), type = "l")
lines(smooth.spline(time(diff(gasf$output)), diff(gasf$output), spar = 0.5), lwd = 2, col = 4)
lines(smooth.spline(time(diff(gasf$output)), diff(gasf$output), spar = 1), lwd = 2, col = 4)



mean(diff(gasf$input))

mean(diff(gasf$output))




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial-correaltion
# ------------------------------------------------------------------------------

graphics.off()

astsa::acf2(gasf$input, max.lag = 50, main = "Input")



# ----------
astsa::acf2(diff(gasf$input), max.lag = 50, main = "Input")


astsa::sarima(p = 3, d = 1, q = 2, xdata = gasf$input, no.constant = TRUE)




# ----------
astsa::acf2(gasf$output, max.lag = 50, main = "Output")



# ----------
astsa::acf2(diff(gasf$output), max.lag = 50, main = "output")


astsa::sarima(p = 3, d = 1, q = 2, xdata = gasf$output, no.constant = TRUE)




# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------

input <- gasf$input
output <- gasf$output


ccf(output, input, lag = 50)


acf(cbind(output, input))



# -->
# output has negative correlation peaks at lag 5 against input




# ----------
tmp <- data.frame(output = diff(gasf$output),  input = diff(gasf$input))

Acf(tmp, lag = 20)




# ------------------------------------------------------------------------------
# lag plot:  real situation of cross-correlation
# ------------------------------------------------------------------------------

astsa::lag2.plot(input, output, max.lag = 10)





# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------


# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


input <- gasf$input

output <- gasf$output


mod_l <- astsa::LagReg(input = input, output = output, L = 3, M = 40, threshold = 0.05)



# -->
# cross-correlated at minus lags
# incorporating only positive lags does not produce good fit.




# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------

# lag operation
length(input)

stats::lag(input, -3)



gasf_t <- ts.intersect(output = stats::lag(output, 0),
                       i0 = stats::lag(input, 0),
                       i1 = stats::lag(input, -1),
                       i2 = stats::lag(input, -2),
                       i3 = stats::lag(input, -3),
                       i4 = stats::lag(input, -4),
                       i5 = stats::lag(input, -5),
                       i6 = stats::lag(input, -6))


head(gasf_t)


nrow(gasf_t)




# ----------
# laggged regression with i0 ~ i6

( u <- lm(output ~ i0 + i1 + i2 + i3 + i4 + i5 + i6, data = gasf_t, na.action = NULL) )


summary(u)



# ----------
( u <- lm(output ~ i6, data = gasf_t, na.action = NULL) )


summary(u)



# ----------
# assess residuals

car::residualPlots(u)


acf2(resid(u))


# -->
# note that residuals have ARIMA(p = 2, d = 0, q = 3)




# ------------------------------------------------------------------------------
# incorporating ARIMA(2,0,3)
# ------------------------------------------------------------------------------

arx <- sarima(gasf_t[,1], 2, 0, 3, xreg = gasf_t[,c(7)])


arx$ttable



# -->
# p-value < 0.05


arx$fit




# ----------
# output estimated by input

pred <- output[-c(1:6)] - resid(arx$fit)

par(mfrow = c(1,1))

ts.plot(pred, output[-c(1:6)], col = c('black', "gray"), lwd = c(2,2))




# ------------------------------------------------------------------------------
# Transfer Function Model
# ------------------------------------------------------------------------------

input <- gasf$input

output <- gasf$output



#############################
# pre-whitening of input

# check auto-correlation and partial-autocorrelation
acf2(input, max.lag = 50)


# apply Ar(3) and take residuals
armod <- arima(input, order = c(3, 0, 0), include.mean = FALSE)


input.pw <- resid(armod)


# ----------
acf2(input.pw, max.lag = 50)




#############################
# filter output by similar transformation AR(3)


# extract AR(3) coefficients
ar1 <- as.numeric(coef(armod)[1])

ar2 <- as.numeric(coef(armod)[2])

ar3 <- as.numeric(coef(armod)[3])


ar1;  ar2;  ar3;

armod$sigma2



# ----------
# filtering output by AR(3)
# sides = 1:  filter coefficients are for past values only
output.fil <- stats::filter(output, filter = c(1, -ar1, -ar2, -ar3), sides = 1)



# ----------
( sa <- sqrt(armod$sigma2) )

( sb <- sd(output.fil, na.rm = TRUE) )


sb / sa



# ----------
graphics.off()
par(mfrow = c(2,1))

ts.plot(cbind(input, input.pw), type = "l", col = c("gray", "black"))

ts.plot(cbind(output, output.fil), type = "l", col = c("gray", "black"))




#############################
# Check cross-correlation between output.fil vs. input.pw


par(mfrow = c(1,1))

tmp_ccf <- ccf(output.fil, input.pw, ylab = "CCF", na.action = na.omit, panel.first = grid())



# -->
# cross correlations up to lag +2 and from lag +8 assumed equal to zero



#############################
# impulse response function:  output.fil vs. input.pw

# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


mod_l2 <- astsa::LagReg(input = input.pw[4:296], output = output.fil[4:296], L = 3, M = 40)




#############################
# Lagged regression


# ----------
ts_gas <- ts.intersect(Output = output.fil, 
                       input3 = stats::lag(input.pw, -3), dframe = T)


u <- lm(Output ~ input3, data = ts_gas)

summary(u)



# -->
# note that coefficient of input3 = -0.55438
# = cross-correlation at lag + 3 * sb / sa = tmp_ccf$acf[25] * sb / sa



# ----------
# here object is not output.fil, but output
output_t <- ts(output, start = 1, end = 296, frequency = 1)

ts_gas <- ts.intersect(Output = output_t,
                       input3 = stats::lag(input.pw, -3), input4 = stats::lag(input.pw, -4),
                       input5 = stats::lag(input.pw, -5), input6 = stats::lag(input.pw, -6), 
                       input7 = stats::lag(input.pw, -7), dframe = T)


u <- lm(Output ~ input3 + input4 + input5 + input6 + input7, data = ts_gas)

summary(u)



u <- lm(Output ~ input5 + input6 + input7, data = ts_gas)

summary(u)




# ----------
pred0 <- ts_gas[,1] - resid(u)

# length is 289 = 296 - 7
length(pred0)

par(mfrow = c(1,1))

ts.plot(pred0, output_t, col = c('black', "gray"), lwd = c(2,2))



# ----------
acf2(resid(u))


# -->
# suggesting add ARIMA(2,0,15) errors

# car::residualPlots(u)



#############################
# with autocorrelated errors


arx2 <- sarima(ts_gas[,1], 2, 0, 15, xreg = ts_gas[,c(4,5,6)])

arx2



# excluding MA terms

arx2 <- sarima(ts_gas[,1], 2, 0, 1, xreg = ts_gas[,c(4,5,6)])

arx2



# ---------
# estimated
pred2 <- ts_gas[,1] - resid(arx2$fit)

# length is 290 = 296 - 6
length(pred)

par(mfrow = c(1,1))

ts.plot(pred2, output_t, col = c('black', "gray"), lwd = c(2,2))




# ----------
# estimated in original series

# phi <- c(ar1, ar2, ar3)

# lag.max <- 20

# convert AR coefficient to MA coefficient and convolution (MA)
# pred_ori <- stats::filter(pred2, filter = c(ARMAtoMA(ar = phi, lag.max = lag.max)), 
#                          method = "convolution", sides = 1)

# output_t <- as.ts(gasf$output, start = 1, end = 295, frequency = 1)

# gap <- output_t - pred_ori

# par(mfrow = c(1,1))

# ts.plot(ts.intersect(cbind(pred_ori + gap[lag.max], output_t)),
#                     col = c('black', "gray"), lwd = c(2,2))




# ------------------------------------------------------------------------------
# Model comparison
# ------------------------------------------------------------------------------

arx$fit


arx2$fit


arx$AIC


arx2$AIC
