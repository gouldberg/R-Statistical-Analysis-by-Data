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
# Fit Vector AR model (VAR)
# ------------------------------------------------------------------------------

# vars Fit vector AR models via least squares.
library(vars)


input <- gasf$input

output <- gasf$output


# later, we test granger causality, so the order is "input" --> "output"
x <- cbind(input, output)



# ----------
# "both" fits constant + trend

VARselect(x, lag.max = 10, type = "both")



# -->
# Note that BIC picks the order p = 3 model while AIC and FPE pick an order p = 6 model
# and Hannan-Quinn selects an order p = 4 model.



# ----------
# Fit VAR model by p = 3  with const + trend

summary(fit3 <- VAR(x, p = 3, type = "both"))




# ----------
# Fit VAR model by p = 6  with const + trend

summary(fit6 <- VAR(x, p = 6, type = "both"))


summary(fit6)


coef(fit6)$output





# ------------------------------------------------------------------------------
# model diagnostics
# ------------------------------------------------------------------------------

graphics.off()



# ----------
# residulas auto-correlation
# Acf2 can not be used for multivariate time series ...

acf(resid(fit3), lag.max = 50)




# ----------
# Examine the multivariate version of the Q-test

vars::serial.test(fit3, lags.pt = 10, type = "PT.adjusted")



# -->
# the Q-test rejects the null hypothesis that the noise is white  --> noise is NOT white




# ----------
# plot diagnostics plot and estimated values

graphics.off()

plot(fit3)



# -->
# for output, residuals are not white




# ------------------------------------------------------------------------------
# Granger causality testing by vars::causality()
# ------------------------------------------------------------------------------

# Granger causality H0:  "input" do not Granger-cause "output" --> REJECTED
# H0: No instantaneous causality between input and output --> NOT REJECTED


vars::causality(fit3, cause = "input")




# ----------
# Granger causality H0:  "output" do not Granger-cause "input" --> NOT REJECTED
# H0: No instantaneous causality between output and input --> NOT REJECTED

x2 <- cbind(output, input)

fit6_2 <- VAR(x2, p = 3, type = "both")

summary(fit6_2)


vars::causality(fit6_2, cause = "output")



