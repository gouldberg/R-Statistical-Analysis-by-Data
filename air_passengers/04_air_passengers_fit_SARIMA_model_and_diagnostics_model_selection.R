setwd("//media//kswada//MyFiles//R//air_passengers")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Air Passengers
#   - Monthly totals of international airline passengers, 1949 to 1960, taken from Box and Jenkins.
# ------------------------------------------------------------------------------

data(AirPassengers, package = "datasets")

str(AirPassengers)

head(AirPassengers)


# ----------
x <- AirPassengers

lx <- log(x)

dlx <- diff(lx)

ddlx <- diff(dlx, 12)



# ------------------------------------------------------------------------------
# ARIMA(1,1,1) * (0,1,1)(12) on the logged data
# ------------------------------------------------------------------------------

mod0 <- sarima(lx, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)


mod0


# -->
# sma1 is highly significant
# AR parameter is NOT significant, so we should try dropping one parameter from the within seasons part.




# ------------------------------------------------------------------------------
# ARIMA(0,1,1) * (0,1,1)(12) and 
# ARIMA(1,1,0) * (0,1,1)(12) model
# ------------------------------------------------------------------------------

mod1 <- sarima(lx, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)


mod2 <- sarima(lx, p = 1, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)


mod0$AIC
mod1$AIC
mod2$AIC


mod0$AICc
mod1$AICc
mod2$AICc



# -->
# All information criteria prefer ARIMA(0,1,1) * (0,1,1)(12) model
# Residual diagnostics show one or two outliers, but the model seems to fit well.


