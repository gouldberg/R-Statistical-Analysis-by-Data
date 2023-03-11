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
# An ARDL(2,1) Unemployment Model including US Growth Rate by lm()
# ------------------------------------------------------------------------------
u1 <- stats::lag(u.ts, -1)

u2 <- stats::lag(u.ts, -2)

g1 <- stats::lag(g.ts, -1)

df <- ts.union(u.ts, u1, u2, g1)



# ----------
us.lm <- lm(u.ts ~ u1 + u2 + g1, data = df)

summary(us.lm)



# ------------------------------------------------------------------------------
# Forecast, forecast error, and forecast interval for T+1, T+2, T+3
# ------------------------------------------------------------------------------

T <- length(u.ts)


# ----------
# for T+1
df1 <- data.frame(u1 = u.ts[T], u2 = u.ts[T-1], g1 = g.ts[T])
uT1 <- predict(us.lm, newdata = df1)
sigT1 <- stats::sigma(us.lm)
lwbT1 <- uT1 - 1.9689 * sigT1
upbT1 <- uT1 + 1.9689 * sigT1



# ----------
# for T+2
df2 <- data.frame(u1 = uT1, u2 = df[T,1], g1 = 0.869)
uT2 <- predict(us.lm, newdata = df2)
sigT2 <- sigT1 * sqrt(1 + coef(us.lm)[["u1"]]^2)
lwbT2 <- uT2 - 1.9689 * sigT2
upbT2 <- uT2 + 1.9689 * sigT2



# ----------
# for T+3
df3 <- data.frame(u1 = uT2, u2 = uT1, g1 = 1.069)
uT3 <- predict(us.lm, newdata = df3)
sigT3 <- sigT1 * sqrt((coef(us.lm)[["u1"]]^2 + coef(us.lm)[["u2"]])^2 + coef(us.lm)[["u1"]]^2 + 1)
lwbT3 <- uT3 - 1.9689 * sigT3
upbT3 <- uT3 + 1.9689 * sigT3



# ----------
kable(tibble(
  Quarter = c("2016Q2", "2016Q3", "2016Q4"),
  Forecast = c(uT1, uT2, uT3),
  "Std. Err. of Forecast" = c(sigT1, sigT2, sigT3),
  "Lower Bound" = c(lwbT1, lwbT2, lwbT3),
  "Upper Bound" = c(upbT1, upbT2, upbT3)),
  digits = 3, align = "c", caption = "Forecast for Unemployment from ARDL(2,1)")



# ------------------------------------------------------------------------------
# An ARDL(2,1) Unemployment Model including US Growth Rate by Arima()
# ------------------------------------------------------------------------------

# An ARDL(2,1) model requires the argument order = c(2, 0, 0)
# Additional regressors are given by xreg()
uArima <- Arima(u.ts, order = c(2, 0, 0), xreg = stats::lag(g.ts, -1))

uf <- forecast(uArima, xreg = c(g.ts[length(g.ts)], 0.869, 1.069), level = 95)



# ----------
tbl <- data.frame(uf)

kable(tbl, digits = 3, caption = "Unemployment Forecasts with Arima")


# -->
# The results seem to be slightly different from the model by lm()


# ----------
par(cex = 1.1, lwd = 1.8, mfrow=c(1,1))
plot(uf, main = "", xlab = "Year", ylab = "Unemployment Rate")

