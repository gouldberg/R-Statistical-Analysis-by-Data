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
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1, 1))

# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.
astsa::acf2(EQcount, 48, main = "Annual count of major earthquakes")



# ----------
astsa::acf2(diff(EQcount), 48, main = "Annual count of major earthquakes")



# -->
# The ACF and PACF seems to be consistent with the behaviour of an ARIMA(2,1,1) ...
# but the ACF is not tailing off ...

# It would be possible to take into account the overdispersion
# by using distributions for counts such as the negative binomial distribution
# or a mixture of Poisson distributions.
# This approach, however, ignores the sample ACF and PACF, which indicate the observations are serially correlated.

