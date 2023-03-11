setwd("//media//kswada//MyFiles//R//chicago")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)

# ------------------------------------------------------------------------------
# data:  chicago
# ------------------------------------------------------------------------------
data(chicago, package = "gamair")

str(chicago)



# ------------------------------------------------------------------------------
# Poission GAM, more flexble model
#   - replace the linear dependencies on the air quality covariates with smooth functions
# ------------------------------------------------------------------------------

ap1 <- gam(death ~ s(time, bs = "cr", k = 200) + s(pm10median, bs = "cr") + s(so2median, bs = "cr") + s(o3median, bs = "cr") + s(tmpd, bs = "cr"), data = chicago, family = poisson)


gam.check(ap1)


# -->
# but almost indistinguishable from those of ap0



# ----------
summary(ap1)



# ----------
par(mfrow = c(1, 1), mar = c(4,4,4,4))
plot(ap1)


# -->
# Plot of estimated smooths indicates a problem with the distribution of pm10median values, in particular, which might be expected to cause
# leverage problems.



# ------------------------------------------------------------------------------
# More detailed examination of the data, surrounding the 4 day mortality surge
# ------------------------------------------------------------------------------

var_interest <- c("death", "pm10median", "o3median", "so2median", "tmpd")

matplot(scale(chicago[3100:3140, var_interest]), type = "l")

chicago[3100:3140, var_interest]



# -->
# The highest temperatures in the temperature record were recorded in the few days preceding the high mortalities,
# then there were also high ozone levels recorded.
# This suggests that average temperature and pollution levels, over the few days preceding a given mortality rate,
# might better predict it than the temperature and levels only on the day itself.

# It seems more plausible that any effects would take some time to manifest themseleves via, for example, the aggravation of existing medical conditions.

