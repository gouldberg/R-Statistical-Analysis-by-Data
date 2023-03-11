setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)




# ------------------------------------------------------------------------------
# Fit ARIMA(0,0,0) * (0,1,1)12 to prec and inf and take residuals for transformation
# ------------------------------------------------------------------------------


# transform prec

fit.prec <- sarima(prec, p = 0, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 12)



prec.t <- resid(fit.prec$fit)





# ----------
# transfrom inf


fit.inf <- sarima(inf, p = 0, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 12)



inf.t <- resid(fit.inf$fit)





# ------------------------------------------------------------------------------
# Prewhitened flow residuals
# ------------------------------------------------------------------------------


( sma_coef <- fit.prec$ttable[1,1] )


inf.pw <- stats::filter(inf, filter = c(1, rep(0, 11), -sma_coef), sides = 1)





# ------------------------------------------------------------------------------
# Cross-correlation
# ------------------------------------------------------------------------------


# This is better profile:  prec.t vs. inf.pw

acf(ts.intersect(cbind(prec.t, inf.pw)), ylab = "CCF", na.action = na.omit, panel.first = grid())




# ----------
acf(ts.intersect(cbind(prec.t, inf.t)), ylab = "CCF", na.action = na.omit, panel.first = grid())


acf(cbind(prec, inf))





# ------------------------------------------------------------------------------
# model for prec.t and prewhiten prec.t
# ------------------------------------------------------------------------------


acf2(prec.t, max.lag = 50)



sarima(prec.t, p = 1, d = 0, q = 0)



fit.prec.t <- arima(prec.t, order = c(1, 0, 0))



# ----------

prec.t.pw <- resid(fit.prec.t)






# ------------------------------------------------------------------------------
# Apply same transformation to inf.t
# ------------------------------------------------------------------------------


( ar1 <- coef(fit.prec.t)[1] )


inf.t.fil <- stats::filter(inf.t, filter = c(1, -ar1), sides = 1)





# ------------------------------------------------------------------------------
# Cross-correlation
# ------------------------------------------------------------------------------


acf(ts.intersect(cbind(prec.t.pw, inf.t.fil)), ylab = "CCF", na.action = na.omit, panel.first = grid())





# ------------------------------------------------------------------------------
# impulse response function
# ------------------------------------------------------------------------------


graphics.off()


L <- 5

mod_lag <- astsa::LagReg(input = prec.t.pw[2:454], output = inf.t.fil[2:454], L = L, M = 100, threshold = 0.0001)





# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------


inf.t.fil <- as.ts(inf.t.fil)

prec.t.pw <- as.ts(prec.t.pw)


clim.t.pw <- ts.intersect(inf = inf, Inf1 = stats::lag(inf, -1), 
                          prec = stats::lag(prec.t.pw, 0))



( u.t.pw <- lm(clim.t.pw[,1] ~ clim.t.pw[,2:3], na.action = na.omit) )



summary(u.t.pw)



coef(u.t.pw)





# ------------------------------------------------------------------------------
# Asssess residuals
# ------------------------------------------------------------------------------


acf2(resid(u.t.pw))




# ------------------------------------------------------------------------------
# ARIMAX model
# ------------------------------------------------------------------------------


mod10 <- sarima(clim.t.pw[,1], 12, 0, 12, P = 0, D = 1, Q = 1, S = 12, xreg = clim.t.pw[,2:3])


mod10




# ------------------------------------------------------------------------------
# 1-step-ahead predictions
# ------------------------------------------------------------------------------


pred <- inf.t.fil + resid(mod10$fit)

graphics.off()

par(mfrow = c(1,1))
ts.plot(pred, inf.t.fil, col = c('skyblue', "black"), lwd = c(2,2))




