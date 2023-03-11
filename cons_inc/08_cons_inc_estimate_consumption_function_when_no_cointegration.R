# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/cons_inc")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cons_inc
#    - y:  permanent income, an infinite stream of income
#    - cons:  consumption
# ------------------------------------------------------------------------------
data("cons_inc", package = "POE5Rdata")

data <- cons_inc

glimpse(data)

str(data)


# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,4)], start = c(1959, 3), end = c(2016, 3), frequency = 4)


# ----------
cons <- ts(cons_inc$cons, start = c(1959,3), end = c(2016,3), frequency = 4)
incm <- ts(cons_inc$y, start = c(1959,3), end = c(2016,3), frequency = 4)


# ----------
# windowed
cons_w <- window(cons, start = c(1985, 1), end = c(2016, 3))
incm_w <- window(incm, start = c(1985, 1), end = c(2016, 3))



# ------------------------------------------------------------------------------
# Consumption function when there is no cointegration
# ------------------------------------------------------------------------------
# Since the consumption and income series are neither trend stationary nor cointegrated,
# we shall regress consumption on income in first differences
# (you know that 1st differences are stationary)


dcons <- dynlm(d(cons_w) ~ d(incm_w) + L(d(cons_w)))

kable(tidy(dcons), digits = 3, align = "c", caption = "Cosumption -- Income Equation in Differences")


# -->
# But please note that this is the model to limited time window.








