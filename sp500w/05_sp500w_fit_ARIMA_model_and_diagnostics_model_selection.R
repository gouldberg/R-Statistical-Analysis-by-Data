setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)


# convert to ts object
sp500w_c <- ts(sp500w, start = 2003, freq = 52)



# ------------------------------------------------------------------------------
# ARMA(15, 12)
# ------------------------------------------------------------------------------

library(astsa)

sarima(sp500w_c, p = 15, d = 0, q = 12, no.constant = FALSE)


# -->
# Does NOT FIT !!!


