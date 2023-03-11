setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\glass")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  glass
#   - Output:  Thickness
#   - Input:   Speed
# ------------------------------------------------------------------------------


glass_y <- read.table("glass_y.txt", sep = "", header = F, colClasses = "numeric")

glass_u <- read.table("glass_u.txt", sep = "", header = F, colClasses = "numeric")


glass <- cbind(glass_u, glass_y)


colnames(glass) <- c("input", "output")



head(glass)





# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

MTS::MTSplot(glass)





# ------------------------------------------------------------------------------
# data exploration:  Smoothing
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(2,1))


plot(glass$output, type = "l")
lines(smooth.spline(time(glass$output), glass$output, spar = 0.5), lwd = 2, col = 4)
lines(smooth.spline(time(glass$output), glass$output, spar = 1), lwd = 2, col = 2)



plot(glass$input, type = "l")
lines(smooth.spline(time(glass$input), glass$input, spar = 0.5), lwd = 2, col = 4)
lines(smooth.spline(time(glass$input), glass$input, spar = 1), lwd = 2, col = 2)





# ------------------------------------------------------------------------------
# forecast::ndiffs and differenced series
# ------------------------------------------------------------------------------

apply(glass, 2, forecast::ndiffs)



mean(glass$input)

mean(glass$output)


