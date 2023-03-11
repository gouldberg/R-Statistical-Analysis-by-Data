setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\my1ef")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MYE1F
# ------------------------------------------------------------------------------


dat <- read.table("MYE1F.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$x





# ------------------------------------------------------------------------------
# time series transformation
# ------------------------------------------------------------------------------


n <- length(dat)

yy <- rep(0, n)

for(i in 2:n) yy[i] <- dat[i] - 0.5 * dat[i-1]

# y <- yy[seq(1, n, by = 2)]

y <- yy




# ------------------------------------------------------------------------------
# Estimate Time-Varying Variance and normalize time series
# ------------------------------------------------------------------------------

library(TSSS)


# estimate time-varying variance

output <- tvvar(y, trend.order = 2)



# -->
# this function reduces to data points in half --> 1300 points




# ----------
graphics.off()


par(mfrow = c(2,2))

# transformed data
plot(output$sm, type = "l", main = "transformed data")
abline(v = c(315, 513), lty = 1, col = "darkgray")

# trend of transformed data
ts.plot(output$trend[,2], type = "l", main = "trend of transformed data")
abline(v = c(315, 513), lty = 1, col = "darkgray")


# time-varying variance = exp(trend)
plot(output$tvv, type = "l", main = "time-varying variance")
abline(v = c(315, 513), lty = 1, col = "darkgray")

# plot(exp(output$trend[,2]), type = "l", main = "time-varying variance")


# normalized data
plot(output$nordata, type = "h", main = "normalized data")
abline(v = c(315, 513), lty = 1, col = "darkgray")




# ----------
# normalized data
dat_norm <- output$nordata




# -----------
# note that variance of normalized series (sigma2) is close to 1

output$sigma2

n <- length(output$sm)

var(output$nordata) * (n - 1) / n



# ----------
# tau2 is 0.000061

output$tau2
