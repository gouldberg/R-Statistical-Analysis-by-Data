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
# Regression with lagged variables:  Transfer function model
# Check cross-correlation
# ------------------------------------------------------------------------------

# Sample CCF of the prewhitened precipitaion and the similarly transformed inflow

par(mfrow = c(1,1))

ccf(prec.pw, inf.fil, ylab = "PrePrec vs fillInflow", na.action = na.omit, panel.first = grid())



# -->
# Negative lags indicate that precipitation leads inflow  --> 0 and -1 are large




# ------------------------------------------------------------------------------
# Check ACF and PACF for inf
# ------------------------------------------------------------------------------


acf2(inf)



# -->
# suggesting AR(1) or AR(2)



# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------

# note the response is infl (not infl.fil)

infl <- ts.intersect(I = inf, IL = stats::lag(inf, -1),  PL0 = prec.pw,  PL1 = stats::lag(prec.pw, -1))
( u <- lm(infl[,1] ~ infl[,2:4], na.action = NULL) )


# infl <- ts.intersect(I = inf, IL = stats::lag(inf, -1),  PL0 = pre.pw)
# ( u <- lm(infl[,1] ~ infl[,2:3], na.action = NULL) )


# infl <- ts.intersect(I = inf, IL = stats::lag(inf, -1),  PL1 = stats::lag(prec.pw, -1))
# ( u <- lm(infl[,1] ~ infl[,2:3], na.action = NULL) )



summary(u)




# ------------------------------------------------------------------------------
# Asssess residuals
# ------------------------------------------------------------------------------

acf2(resid(u))




# -->
# the residuals have some autocorrelation
# residuals can be modeled as AR(1) model or ARMA(1, 1)




# ------------------------------------------------------------------------------
# ARIMAX model
# ------------------------------------------------------------------------------

# with autocorrelated errors: AR(1)

# mod0 <- sarima(infl[,1], 1, 0, 0, xreg = infl[,2:3])
mod0 <- sarima(infl[,1], 1, 0, 0, xreg = infl[,2:4])


mod0


# -->
# sigma^2 = 0.06822



# ----------
# with autocorrelated errors: ARMA(1, 1)

# mod1 <- sarima(infl[,1], 1, 0, 1, xreg = infl[,2:3])
mod1 <- sarima(infl[,1], 1, 0, 1, xreg = infl[,2:4])


mod1


# -->
# sigma^2 = 0.06806



mod0$AIC;  mod1$AIC;




# ------------------------------------------------------------------------------
# 1-step-ahead predictions
# ------------------------------------------------------------------------------

pred <- inf + resid(mod0$fit)


graphics.off()

par(mfrow = c(1,1))
ts.plot(pred, inf, col = c('skyblue', "black"), lwd = c(7,2))




