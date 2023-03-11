setwd("//media//kswada//MyFiles//R//swer")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Extreme rainfall in Switzerland
# ------------------------------------------------------------------------------

data(swer, package = "gamair")

str(swer)


head(swer)

car::some(swer)



# ------------------------------------------------------------------------------
# Initial model by GEV distribution
#
#   - Generalized extreme value distribution:
#      - when epsilon != 0:  t(y) ^ (epsilon + 1) * e(-t(t)) / sigma  where t(y) = [1 + { (y - mu) / sigma } * epsilon] ^ (-1/epsilon)
#      - when epsilon == 0:  t(y) = exp(- (y - mu) / sigma )
#   - The gevlss family in mgcv allows mu, log(sigma), and epsilon to depend on smooth linear predictors.
#   - Maximum likelihood estimation for the GEV distribution is consistent when epsilon > -1, and the GEV distribution has finite variance when epsilon < 0.5,
#     so mgcv uses a modified logit link to keep epsilon in the interval (-1, 0.5)
# ------------------------------------------------------------------------------

# The annual maximum 12 hourly rainfall, exra, is modelled using a GEV distribution, in which the location parameter, mu,
# depends on a space-time interaction term, which of 11 climate.regsions the station belongs to, station elevation in metres and
# the annual North Atlantic Oscillation index nao.
# The nao measures pressure anomalies between the Azores and Iceland, and is somewhat predictive of whether western Europe is dominated by warm wet
# weather from the Atlantic, or colder continental weather.

# Scale parameter, log(sigma), is modelled as depending on year and location (N and E) additively, but is otherwise as the location parameter.
# Shape parameter, epsilon, is modelled as depending on elevation and climate.region.


# d = c(2, 1):  array of marginl basis dimensions, 2 dimensional t.p.r.s basis + 1 dimensional basis
# k = c(20, 5):  dimension(s) of the bases used to represent the smooth term

library(mgcv)

b0 <- gam(list(exra ~ s(nao) + s(elevation) + climate.region + te(N, E, year, d = c(2,1), k = c(20, 5)),
               ~ s(year) + s(nao) + s(elevation) + climate.region + s(N, E),
               ~ s(elevation) + climate.region),
          family = gevlss, data = swer)



# ----------
summary(b0)



# -->
# The choice of initial model reflects the fact that the data tend to be more informative about location than scale, and about scale than shape.



# ----------
graphics.off()

par(mfrow = c(3,3))
plot(b0, scale = 0, scheme = c(1,1,3,1,1,3), contour.col = "white")



