setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\gasfurnace")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
#   - Series of input and output measurements on a fas furnace that have been previously modeled by spectral analysis
#     in Jenkins and Watts (1968), and by lagged correlation methods in Box and Jenkins (1970).
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)




# ------------------------------------------------------------------------------
# Check cross correlation
# ------------------------------------------------------------------------------

# check cross correlation first

par(mfrow = c(1,1))


ccf(output, input, lag = 20)




# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------


gasf_t <- ts.intersect(output = stats::lag(output, 0),
                       i3 = stats::lag(input, -3),
                       i4 = stats::lag(input, -4),
                       i5 = stats::lag(input, -5),
                       i6 = stats::lag(input, -6))



( u <- lm(output ~ i3 + i4 + i5 + i6, data = gasf_t, na.action = NULL) )

( u <- lm(output ~ i3 + i6, data = gasf_t, na.action = NULL) )


summary(u)



coef(u)





# ------------------------------------------------------------------------------
# Assess residuals
# ------------------------------------------------------------------------------

acf2(resid(u))




# ------------------------------------------------------------------------------
# ARIMAX model
# ------------------------------------------------------------------------------

arx <- sarima(gasf_t[,1], 2, 0, 5, xreg = gasf_t[,c(2,5)])





# ------------------------------------------------------------------------------
# 1-step-ahead predictions
# ------------------------------------------------------------------------------

pred <- output[-c(1:5)] + resid(arx$fit)

par(mfrow = c(1,1))

ts.plot(pred, output[-c(1:5)], col = c('skyblue', "black"), lwd = c(7,2))


