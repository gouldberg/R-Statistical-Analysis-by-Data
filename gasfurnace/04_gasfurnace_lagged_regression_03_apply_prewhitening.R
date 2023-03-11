
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)




# ------------------------------------------------------------------------------
# Prewhitening input
# ------------------------------------------------------------------------------

input <- gasf$input

output <- gasf$output



# check auto-correlation and partial-autocorrelation
acf2(input, max.lag = 50)



# apply Ar(3) and take residuals
armod <- arima(input, order = c(3, 0, 0))


input.pw <- resid(armod)


acf2(input.pw, max.lag = 50)




# ------------------------------------------------------------------------------
# filter output by similar transformation
# ------------------------------------------------------------------------------

ar1 <- as.numeric(coef(armod)[1])

ar2 <- as.numeric(coef(armod)[2])

ar3 <- as.numeric(coef(armod)[3])



output.fil <- stats::filter(output, filter = c(1, -ar1, -ar2, -ar3), sides = 1)



# ----------
par(mfrow = c(2,1))

plot(input.pw, type = "l")

plot(output.fil, type = "l")




# ------------------------------------------------------------------------------
# Check cross-correlation
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

ccf(output.fil, input.pw, ylab = "CCF", na.action = na.omit, panel.first = grid())




# ------------------------------------------------------------------------------
# impulse response function
# ------------------------------------------------------------------------------

# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


mod_l2 <- astsa::LagReg(input = input.pw[4:296], output = output.fil[4:296], L = 3, M = 60, threshold = 0.001)




# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------


ts_gas <- ts.intersect(Output = output.fil, input3 = stats::lag(input.pw, -3), input4 = stats::lag(input.pw, -4),
                       input5 = stats::lag(input.pw, -5), input6 = stats::lag(input.pw, -6), input7 = stats::lag(input.pw, -7), dframe = T)


u <- lm(Output ~ input3 + input4 + input5 + input6 + input7, data = ts_gas)


summary(u)


acf2(resid(u))


car::residualPlots(u)



# ------------------------------------------------------------------------------
# with autocorrelated errors
# ------------------------------------------------------------------------------


arx <- sarima(ts_gas[,1], 1, 0, 0, xreg = ts_gas[,c(2,3,4,5,6)])


arx



# ------------------------------------------------------------------------------
# 1-step-ahead predictions
# ------------------------------------------------------------------------------

pred <- ts_gas[,1] + resid(arx$fit)

par(mfrow = c(1,1))

ts.plot(pred, output.fil, col = c('skyblue', "black"), lwd = c(2,2))




# ----------

pred_ori <- stats::filter(pred, filter = c(1, ar1, ar2, ar3), sides = 1)

pred_ori <- as.ts(c(pred_ori), start = 1, end = 290, frequency = 1)

par(mfrow = c(1,1))


ts.plot(ts.intersect(cbind(pred_ori, output), col = c('skyblue', "black"), lwd = c(2,2))





