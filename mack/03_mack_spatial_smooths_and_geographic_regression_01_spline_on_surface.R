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
# Apply spline on the sphere
# ------------------------------------------------------------------------------

# We might object that longitude and latitude are not really an isotropic co-ordinate system, so that a thin plate spline might not be appropriate.
# Apply spline on the sphere by bs = "sos"

mack$log.net.area <- log(mack$net.area)


gm_sos <- gam(egg.count ~ s(lon, lat, bs = "sos", k = 100) + s(I(b.depth^.5)) + s(c.dist) + s(temp.20m) + offset(log.net.area),
           data = mack, family = tw, method = "REML", select = TRUE)


gm_sos



# ----------
summary(gm_sos)



# ----------
AIC(gm2, gm_sos)


# -->
# The AIC gets slightly worse



# ----------
# Compare the predicted values from the model by a thin plate spline and the model by a spline on the sphere
zz <- array(NA, 57*57)
zz[mackp$area.index] <- predict(gm2, mackp)

zz_sos <- array(NA, 57*57)
zz_sos[mackp$area.index] <- predict(gm_sos, mackp)


par(mfrow = c(1,2))
image(lon, lat, matrix(zz, 57, 57), col = gray(0:32/32), cex.lab = 1.5, cex.axis = 1.4)
contour(lon, lat, matrix(zz, 57, 57), add = TRUE)
lines(coast$lon, coast$lat, col = 1)

image(lon, lat, matrix(zz_sos, 57, 57), col = gray(0:32/32), cex.lab = 1.5, cex.axis = 1.4)
contour(lon, lat, matrix(zz_sos, 57, 57), add = TRUE)
lines(coast$lon, coast$lat, col = 1)



# -->
# They are similar with each other, but noticeably rougher as a result of the suppression of long-range autocorrelation.
