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
# Gaussian process smooth
#   - in which the autocorrelation function is forced to drop to zero at some point (one degree separation, for example)
#   - Motivation for doing this is that we might avoid the spatial smoother representing long range auto-correlation that
#     would be better represented using the covariates
# ------------------------------------------------------------------------------

# Replace the spatioal smooth in gm2 with s(lon, lat, bs = "gp", k = 100, m = c(1,1))

gmgps <- gam(egg.count ~ s(lon, lat, bs = "gp", k = 100, m = c(1, 1)) + s(I(b.depth^.5)) + s(c.dist) + s(temp.20m) + offset(log.net.area),
           data = mack, family = tw, method = "REML", select = TRUE)


gmgps



# ----------
summary(gmgps)



# ----------
AIC(gm2, gm_sos, gmgps)



# -->
# In fact, AIC only drops by 2 and the estimated remaining effects are almost unchanged



# ----------
graphics.off()
par(mfrow = c(2,2))
plot(gmgps, shade = TRUE)

