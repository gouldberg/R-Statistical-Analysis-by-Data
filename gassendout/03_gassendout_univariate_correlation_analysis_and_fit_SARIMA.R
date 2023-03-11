grasetwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\gassendout")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Sendout
# ------------------------------------------------------------------------------

gas <- read.table("GasSendout.txt", sep = "", header = F, colClasses = "numeric")



colnames(gas) <- c("MetI", "gasSendout", "V3")


head(gas)




# ------------------------------------------------------------------------------
# gasSendout:  Correlation analysis for univariate time series and fit SARIMA
# ------------------------------------------------------------------------------

graphics.off()

astsa::acf2(diff(gas$gasSendout), max.lag = 250)

astsa::acf2(diff(gas$gasSendout, 7), max.lag = 250)

astsa::acf2(diff(diff(gas$gasSendout), 7), max.lag = 250)



# start from ARIMA(1,1,1) * (3,0,1)[7]
astsa::sarima(p = 1, d = 1, q = 1, P = 3, D = 0, Q = 1, S = 7, xdata = gas$gasSendout)

astsa::sarima(p = 1, d = 1, q = 1, P = 3, D = 0, Q = 1, S = 7, xdata = gas$gasSendout, no.constant = TRUE)

# convergence problem
astsa::sarima(p = 1, d = 1, q = 1, P = 2, D = 0, Q = 1, S = 7, xdata = gas$gasSendout, no.constant = TRUE)

astsa::sarima(p = 1, d = 1, q = 2, P = 1, D = 0, Q = 1, S = 7, xdata = gas$gasSendout, no.constant = TRUE)



# ----------
u <- stats::arima(x = gas$gasSendout, order = c(1,1,2), seasonal = list(order = c(1,0,1), period = 7))

acf2(resid(u))


tsdiag(u)

par(mfrow = c(1,1))

plot(gas$gasSendout + resid(u), type = "l", col = "blue")
lines(time(gas$gasSendout), gas$gasSendout, type = "l", col = "black")





# ------------------------------------------------------------------------------
# MetI:  Correlation analysis for univariate time series and fit SARIMA
# ------------------------------------------------------------------------------


graphics.off()

astsa::acf2(gas$MetI, max.lag = 250)

astsa::acf2(diff(gas$MetI), max.lag = 250)

astsa::acf2(diff(gas$MetI, 7), max.lag = 250)

astsa::acf2(diff(diff(gas$MetI), 7), max.lag = 250)



# start from ARIMA(2,1,2) * (0,0,0)
astsa::sarima(p = 2, d = 1, q = 2, P = 0, D = 0, Q = 0, S = 0, xdata = gas$MetI)




# ----------
u <- stats::arima(x = gas$gasSendout, order = c(2,1,2), seasonal = list(order = c(0,0,0), period = 0))

acf2(resid(u))


tsdiag(u)

par(mfrow = c(1,1))

plot(gas$MetI + resid(u), type = "l", col = "blue")
lines(time(gas$MetI), gas$MetI, type = "l", col = "black")




# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------


input <- diff(gasf$input)
output <- diff(gasf$output)


input <- gasf$input
output <- gasf$output


ccf(output, input, lag = 50)


acf(cbind(output, input))



# -->
# output has negative correlation peaks at lag 5




# ----------
tmp <- data.frame(output = diff(gasf$output),  input = diff(gasf$input))

Acf(tmp, lag = 20)




# ----------
astsa::lag2.plot(output, input, max.lag = 20)



