
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)




# ----------
# VAR model

library(vars)


input <- gasf$input

output <- gasf$output


x <- cbind(input, output)

fit3 <- VAR(x, p = 3, type = "both")

fit6 <- VAR(x, p = 6, type = "both")




# ------------------------------------------------------------------------------
# data:  Gas Furnace
# ------------------------------------------------------------------------------


# Acf2 can not be used for multivariate time series ...

acf(resid(fit3), lag.max = 50)


acf(resid(fit6), lag.max = 50)



# -->
# zero-order correlation is large ...

# This means that the AR model is not capturing the concurrent effect




# ------------------------------------------------------------------------------
# Examine the multivariate version of the Q-test
# ------------------------------------------------------------------------------

vars::serial.test(fit6, lags.pt = 10, type = "PT.adjusted")



# -->
# the Q-test DOES NOT rejects the null hypothesis that the noise is white.




# ------------------------------------------------------------------------------
# plot diagnostics plot and estimated values
# ------------------------------------------------------------------------------

graphics.off()

plot(fit6)



