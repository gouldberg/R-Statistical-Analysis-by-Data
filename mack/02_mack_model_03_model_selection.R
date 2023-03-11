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
# Dropping salinity --> drop temp.surf
# ------------------------------------------------------------------------------

# drop salinity
gm1 <- gam(egg.count ~ s(lon, lat, k = 100) + s(I(b.depth^.5)) + s(c.dist) + s(temp.20m) + s(temp.surf) + offset(log.net.area),
            data = mack, family = tw, method = "REML", select = TRUE)

gm1


# -->
# still give a fully penalized temp.surf effect, so that is dropped too.



# ----------
# drop temp.surf
gm2 <- gam(egg.count ~ s(lon, lat, k = 100) + s(I(b.depth^.5)) + s(c.dist) + s(temp.20m) + offset(log.net.area),
           data = mack, family = tw, method = "REML", select = TRUE)

gm2



# ----------
car::residualPlot(gm1)


car::residualPlot(gm2)


# some residual plots
graphics.off()
par(mfrow = c(2,3))
plot(mack$temp.20m, resid(gm2))
plot(mack$lat, resid(gm2))
plot(mack$lon, resid(gm2))
plot(mack$temp.surf, resid(gm2))
plot(mack$c.dist, resid(gm2))
qqnorm(resid(gm2));  qqline(resid(gm2))



# -->
# The plots appear reasonable, with nothing to indicate much wrong with the assumed mean-variance relationship, nor other suspicious patterns.
# The small dense block of residuals on each plot relates to the densely sampled low abundance block in the south of the survey area.



# ----------
# Estimated non-linear effects
par(mfrow = c(2, 2))
plot(coast$lon, coast$lat, type="l", col = "blue")
par(new = T)
plot(gm2, shade = TRUE)



# ----------
summary(gm2)



# -->
# In some respects the high degrees of freedom estimated for the spatial smooth is disappointing: biologically it would be more satisfactory for the model
# to be based on predictors to which spawning fish might be responding directly.
# Spatial location can really only be a proxy for something else, or the result of a process in which much of the pattern is driven by spatial correlation.


