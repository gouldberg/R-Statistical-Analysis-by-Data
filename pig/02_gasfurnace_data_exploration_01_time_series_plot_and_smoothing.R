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


