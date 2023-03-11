setwd("//media//kswada//MyFiles//R//jj")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Johnson & Johnson Quarterly Earnings
# ------------------------------------------------------------------------------

data(jj, package = "astsa")

str(jj)

jj


# ----------
x <- jj

lx <- log(x)

dlx <- diff(lx)

ddlx <- diff(dlx, 4)



# ------------------------------------------------------------------------------
# ARIMA(1,1,1) * (0,1,1)(4) on the logged data
# ------------------------------------------------------------------------------

sarima(lx, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 4)


# -->
# AR parameter is NOT significant, so we should try dropping one parameter from the within seasons part.



# ------------------------------------------------------------------------------
# ARIMA(0,1,1) * (0,1,1)(4) and 
# ARIMA(1,1,0) * (0,1,1)(4) model
# ------------------------------------------------------------------------------

sarima(lx, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 4)


sarima(lx, p = 1, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 4)



# -->
# All information criteria prefer ARIMA(0,1,1) * (0,1,1)(4) model
# Residual diagnostics show one or two outliers, but the model seems to fit well ...


