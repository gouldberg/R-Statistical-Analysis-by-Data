setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics\\weight")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  weight
# ------------------------------------------------------------------------------


dat <- read.table("Weight.dat", header = FALSE)


str(dat)


car::some(dat)



# ----------
colnames(dat) <- c("weight")




# ------------------------------------------------------------------------------
# data exploration:  plot time series
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))

ts.plot(dat$weight, type = "b")




# ------------------------------------------------------------------------------
# data exploration:  smoothing  (spline smoothing)
# ------------------------------------------------------------------------------


ts.plot(dat$weight, type = "b")

lines(smooth.spline(dat$weight, spar = 0.25), lwd = 2, col = "darkgray")

lines(smooth.spline(dat$weight, spar = 0.5), lwd = 2, col = "orange")

lines(smooth.spline(dat$weight, spar = 0.75), lwd = 2, col = "red")

lines(smooth.spline(dat$weight, spar = 1), lwd = 2, col = "blue")




# ------------------------------------------------------------------------------
# Check 1st order difference
# ------------------------------------------------------------------------------


forecast::ndiffs(dat$weight)


ts.plot(diff(dat$weight), type = "b")




# ------------------------------------------------------------------------------
# Unit Root Test:  Augmented Dickey-Fuller Test
# ------------------------------------------------------------------------------

library(urca)


# ----------
# random walk
# diff(y(t)) = pi * y(t-1) + sum{ rho(j) * diff(y(t-j)) } + alpha(t),  j = 1,2,3,4

summary(ur.df(dat$weight, type = "none", lags = 4))


# -->
# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root)
# t test statistics = -0.3036 --> critical value of 5pct = -1.95 --> rejected




# -->
# fabrics may not be random walk (local level model)




# ------------------------------------------------------------------------------
# sample auto-correlation and partial auto-correlation (Correlogram)
# ------------------------------------------------------------------------------


# this is non-stationary time series ...
astsa::acf2(dat$weight, max.lag = 40)


astsa::acf2(diff(dat$weight), max.lag = 40)



