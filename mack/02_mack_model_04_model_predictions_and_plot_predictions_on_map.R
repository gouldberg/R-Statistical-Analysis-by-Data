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
# model predictions
# ------------------------------------------------------------------------------

mackp$log.net.area <- rep(0, nrow(mackp))

lon <- seq(-15, -1, 1/4)

lat <- seq(44, 58, 1/4)



# ----------
zz <- array(NA, 57 * 57)

zz[mackp$area.index] <- predict(gm2, mackp)




# ------------------------------------------------------------------------------
# Simple map of predicted densities
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,1))

image(lon, lat, matrix(zz, 57, 57), col = gray(0:32/32), cex.lab = 1.5, cex.axis = 1.4)
contour(lon, lat, matrix(zz, 57, 57), add = TRUE)
lines(coast$lon, coast$lat, col = 1)



# -->
# Notice tha substantial problem that the egg densities remain high at the western boundary of the survey data.
# (the right side is the western boundary)


