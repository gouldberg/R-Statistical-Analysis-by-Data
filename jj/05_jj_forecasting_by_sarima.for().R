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
# Forecasting by sarima.for
# ------------------------------------------------------------------------------

# next 4 quarters forecast using ARIMA(0,1,1) * (0,1,1)(4) model on the logged data.
par(mfrow=c(1,1))
sarima.for(lx, n.ahead = 4, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 4)

