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
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------


# use corrected time series

par(mfrow = c(1,1))

ccf(output, input, lag = 50)



# -->
# showing non-causal response at negative lags
# maybe contaminated by MetI noise




# differenced series
ccf(diff(output), diff(input), lag = 50)



# original series
ccf(gas$gasSendout, gas$MetI, lag = 50)





# ----------
astsa::lag2.plot(output, input, max.lag = 20)


# at time lag = 0, the regression slope is 1.33
summary(lm(output ~ input))




