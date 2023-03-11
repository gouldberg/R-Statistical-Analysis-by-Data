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



# ----------
par(cex = 1.6, mar = c(4, 4, 1, 2), lwd = 1.5)
ts.plot(cons, ylab = "Consumption", xlab = NULL)
ts.plot(incm, ylab = "Disable Income", xlab = NULL)

par(cex = 1.6, mar = c(4, 4, 1, 2), lwd = 1.5)
ts.plot(cons_w, ylab = "Consumption", xlab = NULL)
ts.plot(incm_w, ylab = "Disable Income", xlab = NULL)



# ------------------------------------------------------------------------------
# Asess stationarity of consumption by intercept and "trend" model
# ------------------------------------------------------------------------------
dc <- dynlm(d(cons) ~ trend(cons) + L(cons) + d(L(cons)))
dc_w <- dynlm(d(cons_w) ~ trend(cons_w) + L(cons_w) + d(L(cons_w)))

kable(tidy(dc), digis = 3, align = "c", caption = "Consumption Model with Intercept and Trend")
kable(tidy(dc_w), digis = 3, align = "c", caption = "Consumption Model with Intercept and Trend")


# -->
# insignificant for whole periods, but significant through windowed limited period



# ------------------------------------------------------------------------------
# Asess stationarity of income by intercept and "trend" model
# ------------------------------------------------------------------------------
dy <- dynlm(d(incm) ~ trend(incm) + L(incm) + d(L(incm)))
dy_w <- dynlm(d(incm_w) ~ trend(incm_w) + L(incm_w) + d(L(incm_w)))

kable(tidy(dy), digis = 3, align = "c", caption = "Disable Income Model with Intercept and Trend")
kable(tidy(dy_w), digis = 3, align = "c", caption = "Disable Income Model with Intercept and Trend")


# -->
# insignificant for whole periods, but significant through windowed limited period



stargazer(dc_w, dy_w, header=FALSE, title = "Consumption and Income in Differences", type = "text",
          column.labels = c("Consumption", "Income"), dep.var.labels.include = TRUE, dep.var.caption = "Dependent variables: Differences")

# -->
# indicating that the null hypothesis of trend non-stationarity cannot be rejected.


