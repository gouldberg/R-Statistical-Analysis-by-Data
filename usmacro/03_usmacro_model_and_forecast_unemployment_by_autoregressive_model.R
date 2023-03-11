# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/usmacro")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  usmacro
#
# u:  U.S. unemployment rate over a period of more than six decades
# g:  U.S. growth rate
# ------------------------------------------------------------------------------
data("usmacro", package = "POE5Rdata")

data <- usmacro

glimpse(data)

str(data)


is.ts(usmacro)

u.ts <- ts(usmacro$u, start = c(1948, 1), end = c(2016, 1), frequency = 4)

g.ts <- ts(usmacro$g, start = c(1948, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# Forecasting Unemployment for three future periods with an AR(2) model (forecast() function)
# ------------------------------------------------------------------------------
# AR(2) model by dynlm()
u.ar2 <- dynlm(u.ts ~ L(u.ts, 1:2))

summary(u.ar2)

kable(tidy(u.ar2), caption = "The AR(2) Unemployment Model", digits = 3)



# ----------
# AR(2) model by ar() and forecast by forecast()
ar2u <- ar(u.ts, aic = TRUE, order.max = 2, method = "ols")


library(forecast)
fcst <- data.frame(forecast(ar2u, 3))


kable(fcst, digits = 3, caption = "Unemployment forcasts with AR(2)", align = "c")



# ----------
graphics.off()
par(cex = 1.2, lwd = 1.8)
plot(forecast(ar2u, 3), main = "", xlab = "Year", ylab = "Unemployment Rate")


