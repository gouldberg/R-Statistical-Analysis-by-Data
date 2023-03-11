setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\nikkei225")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nikkei225
# ------------------------------------------------------------------------------


dat <- read.table("nikkei225.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$nikkei225


dat <- diff(log(dat))




# ------------------------------------------------------------------------------
# Estimate Time-Varying Variance and normalize time series
# ------------------------------------------------------------------------------

library(TSSS)


# ----------
# pre-process
n <- length(dat)
yy <- rep(0, n)
for(i in 2:n) yy[i] <- dat[i] - 0.5 * dat[i-1]
y <- yy[seq(1, n, by = 2)]


output <- tvvar(y, trend.order = 2)




# ----------
graphics.off()


par(mfrow = c(4,1))

# transformed data
plot(output$sm, type = "l", main = "transformed data")


# time-varying variance
plot(output$tvv, type = "l", main = "time-varying variance")


# trend
plot(output$tvv, type = "l", main = "trend")


# normalized data
plot(output$nordata, type = "h", main = "normalized data")




# ----------
dat_norm <- output$nordata




# -----------
# note that sigma2 is close to 1 (normalized)

output$sigma2





# ----------
# tau2 is 0.000122

output$tau2


