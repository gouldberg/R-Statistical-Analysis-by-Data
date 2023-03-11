setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")

str(econ5)


car::some(econ5)



# ----------
# data transformation
# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation function (ACF) and partial-autocorrelation function (PACF)
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


# ----------
# unemployment
acf2(diff(econ5$unemp), max.lag = 20)
acf2(U, max.lag = 20)



# ----------
# gnp
acf2(diff(econ5$gnp), max.lag = 20)
acf2(G, max.lag = 20)



# ----------
# consumption
acf2(diff(econ5$consum), max.lag = 20)
acf2(C, max.lag = 20)
acf2(diff(econ5$consum, 2), max.lag = 20)



# ----------
# government investment
acf2(diff(econ5$govinv), max.lag = 20)



# ----------
# private investment
acf2(diff(econ5$prinv), max.lag = 20)


