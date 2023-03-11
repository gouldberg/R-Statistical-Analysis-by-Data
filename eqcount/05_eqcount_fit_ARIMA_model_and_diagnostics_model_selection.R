setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")

str(EQcount)

EQcount



# ------------------------------------------------------------------------------
# ARIMA(2,1,1)
# ------------------------------------------------------------------------------

library(astsa)

sarima(EQcount, p = 2, d = 1, q = 1, no.constant = TRUE)


# -->
# AR parameter is NOT significant ...




# ----------
sarima(EQcount, p = 1, d = 0, q = 7, no.constant = TRUE)


sarima(EQcount, p = 1, d = 0, q = 2, no.constant = TRUE)


sarima(EQcount, p = 1, d = 0, q = 1, no.constant = TRUE)


sarima(EQcount, p = 3, d = 0, q = 0, no.constant = TRUE)



# -->
# p = 1, d = 0, q = 1  is actually fitted.




# ----------
# check forecast --> but forecast

sarima.for(EQcount, n.ahead = 10, p = 1, d = 0, q = 1, P = 0, D = 0, Q = 0) 

sarima.for(EQcount, n.ahead = 10, p = 1, d = 0, q = 7, P = 0, D = 0, Q = 0) 
