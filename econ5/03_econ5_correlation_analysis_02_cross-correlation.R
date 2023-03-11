setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")


str(econ5)


car:ssome(econ5)



# ----------
# data transformation
# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))




# ------------------------------------------------------------------------------
# Correlation analysis:  Cross-correlation
# ------------------------------------------------------------------------------

Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)


graphics.off()
par(mfrow = c(3,1))

Ccf(Ud, Gd, lag = 20)

Ccf(Ud, Cd, lag = 20)

Ccf(Gd, Cd, lag = 20)



# -->
# 1st difference of Unemployment leads (delay) 5 - 8 time periods to 1st difference of GNP  (in negative way)
# 1st difference of Unemployment lags 0 - 4 time periods to 1st difference of consumption (in negative way)




# ----------
# for reference
graphics.off()
par(mfrow = c(3,1))

Ccf(U, G, lag = 40)

Ccf(U, C, lag = 40)

Ccf(G, C, lag = 40)




# ------------------------------------------------------------------------------
# Correlation analysis:  acf function does all the cross-correlation for multivariate time series
# ------------------------------------------------------------------------------

dat <- cbind(Ud, Gd, Cd)

# dat <- cbind(U, G, C)


acf(dat, lag.max = 20, cex.main = 2)

acf(dat, 20)$acf

