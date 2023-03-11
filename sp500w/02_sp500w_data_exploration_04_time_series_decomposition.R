setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)


# convert to ts object
sp500w_c <- ts(sp500w, start = 2003, freq = 52)




# ------------------------------------------------------------------------------
# data exploration:  time series decomposition by stats::decompose
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


dec <- decompose(sp500w_c, type = "additive")

plot(dec)



# -->
# there are some seasonal patterns too.



# ----------
# diff
dec <- decompose(diff(sp500w_c), type = "additive")

plot(dec)




# ----------
# multiplicative
dec <- decompose(sp500w_c, type = "multiplicative")

plot(dec)



# -->
# "multiplicative" in random component shows some anomaly !!

