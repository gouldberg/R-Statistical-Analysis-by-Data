setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\dry2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dry2
# ------------------------------------------------------------------------------


u <- read.csv("dry2_u.txt", sep = "", header = F, colClasses = "numeric")

y <- read.csv("dry2_y.txt", sep = "", header = F, colClasses = "numeric")


dry2 <- cbind(u, y)


colnames(dry2) <- c("input", "output")


head(dry2)



# ----------
str(dry2)



# ----------
# detrending
output2 <- resid(lm(output ~ time(output), data = dry2))


# demean
dry2$input2 <- dry2$input - mean(dry2$input)




# ----------
# VAR model

library(vars)


input <- dry2$input2

output <- dry2$output


x <- cbind(input, output)

fit5 <- VAR(x, p = 5, type = "both")

fit6 <- VAR(x, p = 6, type = "both")




# ------------------------------------------------------------------------------
# data:  Gas Furnace
# ------------------------------------------------------------------------------


# Acf2 can not be used for multivariate time series ...

acf(resid(fit5), lag.max = 50)


acf(resid(fit6), lag.max = 50)



# -->
# zero-order correlation is large ...

# This means that the AR model is not capturing the concurrent effect




# ------------------------------------------------------------------------------
# Examine the multivariate version of the Q-test
# ------------------------------------------------------------------------------

vars::serial.test(fit5, lags.pt = 10, type = "PT.adjusted")



# -->
# the Q-test DOES rejects the null hypothesis that the noise is white.




# ------------------------------------------------------------------------------
# plot diagnostics plot and estimated values
# ------------------------------------------------------------------------------

graphics.off()

plot(fit6)



