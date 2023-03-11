setwd("//media//kswada//MyFiles//R//mack")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mack
# ------------------------------------------------------------------------------

data(mack, package = "gamair")

data(mackp, package = "gamair")

data(coast, package = "gamair")


str(mack)

str(mackp)

str(coast)



# ----------
# mackp contains prediction grid data for 1992 mackerel egg model.
# A data from with 5 columns. Each row corresponds to one spatial location within the survey area.

# coast:  European coastline from -11 to 0 East and from 43 to 59 North



# ------------------------------------------------------------------------------
# Simple additive structure with tweedie or negative binomial distributions
# ------------------------------------------------------------------------------

# A reasonable model for the mean eff count is:  E(egg.count) = g * net.area
# where g is the density of eggs, per square metre of sea surface, at the ith sampling location.
# --> log(E(egg.count)) = d + log(net.area),  d = log(g) will be modelled and log(net.area) will be treated as a model offset

# Since the response is a count, it is tempting to start with the Poisson distribution, but any attempt to fit the sort of models used here
# with a Poisson response shows very strong evidence of over-dispersion: the residuals are much too large for consistency with a Poisson with unit scale parameter.
# An alternative is to use the quasipoisson family in R, which makes the Poisson-like assumption that var(yi) = phi * E(yi), but without fixing phi = 1.
# However, to facilitate things like AIC model seletion and REML smoothing parameter estimation
# it helps to use full distributions.
# The obvious alternatives are the Tweedie or negative binomial distribution.


mack$log.net.area <- log(mack$net.area)



# ----------
# The square root transofrm of b.depth is to reduce leverage from very high values

# select = TRUE:  add an extra penalty to each term so that it can be penalized to zero.
# This means that the smoothing parameter estimation that is part of fitting can completely remove terms from the model
# If the corresponding smoothing parameter is estimated as zero then the extra penalty has no effect.

gmtw <- gam(egg.count ~ s(lon, lat, k = 100) + s(I(b.depth^.5)) + s(c.dist) + s(salinity) + s(temp.surf) + s(temp.20m) + offset(log.net.area),
            data = mack, family = tw, method = "REML", select = TRUE)


summary(gmtw)



# -->
# Tweedie p is estimated as 1.33




# ----------
# also by negative binomial distribution
gmnb <- gam(egg.count ~ s(lon, lat, k = 100) + s(I(b.depth^.5)) + s(c.dist) + s(salinity) + s(temp.surf) + s(temp.20m) + offset(log.net.area),
            data = mack, family = nb, method = "REML", select = TRUE)


summary(gmnb)



# -->
# theta is estimated as 1.37



# ----------
# residual plot
par(mfrow = c(1, 2))

plot(fitted(gmtw), resid(gmtw))

plot(fitted(gmnb), resid(gmnb))



graphics.off()
par(mfrow = c(2, 2))

car::residualPlot(mod_glm2)

car::residualPlot(gmtw)

car::residualPlot(gmnb)




# -->
# Acceptable behaviour for the deviance residuals of the Tweedie model,
# but a problematic decline in the variance of the deviance residuals with fitted value for the negative binomial model,
# indicating that the negative binomial assumes too sharp an increase in the variance with the mean.



# ----------
AIC(gmtw, gmnb)


# -->
# AIC also give sa much smaller value for the Tweedie model.



# ----------
# Estimated model terms for the simple additive gmtw model

graphics.off()
par(mfrow = c(2,2))

plot(gmtw, shade = TRUE)



# -->
# The salinity and surf.temp effects have been penalized out of the model altogether.
# Note however, that there are many missing values for salinity, so that dropping it allows a much larger set of data to be used for elimination.

