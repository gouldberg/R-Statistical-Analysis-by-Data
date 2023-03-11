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
# Check cross-correlation between inflow and precipitation
# ------------------------------------------------------------------------------

# check cross correlation first

par(mfrow = c(1,1))

ccf(inf, prec, lag = 20, main = "Infow vs Precipitation", ylab = "CCF")




# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------

infl <- ts.intersect(I = inf, IL = stats::lag(inf, 5),  PL0 = prec,  PL1 = stats::lag(prec, -1))


( u <- lm(infl[,1] ~ infl[,2:4], na.action = NULL) )



summary(u)




# ------------------------------------------------------------------------------
# Asssess residuals
# ------------------------------------------------------------------------------

acf2(resid(u))




# -->
# the residuals have some autocorrelation
# residuals can be modeled as AR(2) model or ARMA(2, 5)




# ------------------------------------------------------------------------------
# ARIMAX model
# ------------------------------------------------------------------------------

# with autocorrelated errors: AR(2)

mod0 <- sarima(infl[,1], 2, 0, 0, xreg = infl[,2:4])



# ----------
# with autocorrelated errors: ARMA(2, 5)

mod1 <- sarima(infl[,1], 2, 0, 5, xreg = infl[,2:4])



mod0$AIC;  mod1$AIC;



