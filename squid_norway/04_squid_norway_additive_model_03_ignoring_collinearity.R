setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ------------------------------------------------------------------------------
# Consequences of ignoring collinearity in GAM
# ------------------------------------------------------------------------------
M6 <- gam(d15N ~ s(Lat)+ s(Depth) + s(ML), data = Squid)


# -->
# error occurs because depth and latitude have only a limited number of unique observations.
# The simple solution is to not fit latitude and depth as smoothers.
# Alternatively, set the upper limit for the degrees of freedom for these two smoothers to a small value, e.g. 3 df.



M6 <- gam(d15N ~ s(Lat, k = 4) + s(Depth, k = 4) + s(ML), data = Squid)
summary(M6)



# -->
# The smoother for mantle length is nearly the same.
# This means that the collinearity between latitude and depth did not cause serious problems in the additive model.
# We suspect that because mantle length is the only covariate showing a strong relationship to d15N, the additive model is capable of
# dealing with the collinearity.
# This may not hold true for other examples, and we always caution against applying models with collinear variables, espeically in GAM.

