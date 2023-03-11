setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------


econ5 <- read.table("econ5.txt", header = T, sep = "\t")


str(econ5)


car::some(econ5)



# ----------
# First logging and then removing the linear trend

U <- resid(umod <- lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(gmod <- lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(cmod <- lm(log(econ5$consum) ~ time(log(econ5$consum))))

GI <- resid(gimod <- lm(log(econ5$govinv) ~ time(log(econ5$govinv))))

P <- resid(pmod <- lm(log(econ5$prinv) ~ time(log(econ5$prinv))))


gr <- cbind(U, G, C, GI, P)


MTSplot(gr)




# ------------------------------------------------------------------------------
# auto-correlation and partial-autocorrelation
# cross-correlation
# ------------------------------------------------------------------------------


acf2(gr[,"U"], max.lag = 50)


acf(gr, max.lag = 50)





# ------------------------------------------------------------------------------
# Fit VAR model
# ------------------------------------------------------------------------------


# vars fit vector AR models via least squares.
library(vars)



# ----------
# detrended time series
# "none" fits no constant and no trend


VARselect(gr, lag.max = 10, type = "none")



# -->
# if including all variables, model cannot estimate properly



# ----------
# removing variables

VARselect(gr[,c("U","G","C","P")], lag.max = 10, type = "none")

VARselect(gr[,c("U","G","C")], lag.max = 10, type = "none")



# ----------
# Fit VAR model by p = 2
# but prinv is not signficant

summary(fit <- VAR(gr[,c("U","G","C","P")], p = 2, type = "none"))



# ----------
# Fit VAR model by p = 3

summary(fit <- VAR(gr[,c("U","G","C")], p = 2, type = "none"))



# ----------
# Fit VAR model by p = 2  --> U model is significant
# also Multiple R-squared = 0.9158

summary(fit <- VAR(gr[,c("U","G","C")], p = 2, type = "none"))




# ----------
# Examine the residuals:  cross-correlations of the residulas
# Acf2 can not be used for multivariate time series ...

acf(resid(fit), lag.max = 20)



# Notice that most of the correlations in the residual series are negliible,
# however, note that there are some zero-order correlation
# This means that AR model is not capturing the concurrent effect



# ----------
vars::serial.test(fit, lags.pt = 12, type = "PT.adjusted")


# -->
# Thus, not unexpectedly, the Q-test rejects the null hypothesis that the noise is white.




# ----------
pred_var <- ts(fitted(fit)[,"U"], start = 1) + umod$coef[1] + umod$coef[2] * time(econ5$unemp)[3:161]


par(mfrow = c(1,1), mar = c(2,2,2,2))
plot(pred_var, lwd = 2, col = 4)
points(log(econ5$unemp)[3:161])





# ------------------------------------------------------------------------------
# Fit VARMA model
# ------------------------------------------------------------------------------


library(marima)


model <- define.model(kvar = 3, ar = c(1,2), ma = c(1))


( arp <- model$ar.pattern )

( map <- model$ma.pattern )



fit <- marima(as.matrix(gr[,c("U", "G", "C")]), ar.pattern = arp, ma.pattern = map, means = c(1,1,1), penalty = 1)


fit



# ----------
innov <- t(resid(fit))


plot.ts(innov)


acf(innov, na.action = na.pass)



# ----------

pred_varma <- ts(fitted(fit)[1,], start = 1) + umod$coef[1] + umod$coef[2] * time(econ5$unemp)


par(mfrow = c(1,1), mar = c(2,2,2,2))

plot(pred_varma, lwd = 2, col = "black")

lines(c(NA, NA, pred_var), lwd = 2, col = "blue")

points(log(econ5$unemp))



# ----------
sum(abs(log(econ5$unemp)[3:161] - pred_varma[3:161])) / 159

sum(abs(log(econ5$unemp)[3:161] - pred_var)) / 159



# -->
# VAR model is sligtly better than VARMA model


