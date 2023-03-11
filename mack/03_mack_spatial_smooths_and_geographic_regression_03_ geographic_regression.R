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
# Geographic regression model
#   - This is a type of varying coefficient model in which each covariate is assumed to have a linear influence on the linear predictor for the response,
#     but the slope parameter of that linear dependence varies smoothly with geographic location.
# ------------------------------------------------------------------------------

gamgr <- gam(egg.count ~ s(lon, lat, k = 100) + s(lon, lat, by = temp.20m) + s(lon, lat, by = I(b.depth^.5)) + offset(log.net.area), 
             data = mack, family = tw, method = "ML")


gamgr



# ----------
summary(gamgr)




# ----------
AIC(gm2, gm_sos, gamgr)



# -->
# It has an AIC value 15 lower than gm2, so is worth serious consideration as an alternative model.



# ----------
zz_ggps <- array(NA, 57*57)
zz_ggps[mackp$area.index] <- predict(gmgps, mackp)

zz_gr <- array(NA, 57*57)
zz_gr[mackp$area.index] <- predict(gamgr, mackp)


par(mfrow = c(2,2))
image(lon, lat, matrix(zz, 57, 57), col = gray(0:32/32), cex.lab = 1.5, cex.axis = 1.4)
contour(lon, lat, matrix(zz, 57, 57), add = TRUE)
lines(coast$lon, coast$lat, col = 1)

image(lon, lat, matrix(zz_sos, 57, 57), col = gray(0:32/32), cex.lab = 1.5, cex.axis = 1.4)
contour(lon, lat, matrix(zz_sos, 57, 57), add = TRUE)
lines(coast$lon, coast$lat, col = 1)

image(lon, lat, matrix(zz_ggps, 57, 57), col = gray(0:32/32), cex.lab = 1.5, cex.axis = 1.4)
contour(lon, lat, matrix(zz_ggps, 57, 57), add = TRUE)
lines(coast$lon, coast$lat, col = 1)

# -->
# Noticeably rougher as a result of the suppression of long-range autocorrelation


image(lon, lat, matrix(zz_gr, 57, 57), col = gray(0:32/32), cex.lab = 1.5, cex.axis = 1.4)
contour(lon, lat, matrix(zz_gr, 57, 57), add = TRUE)
lines(coast$lon, coast$lat, col = 1)

# -->
# Noticeably smoother than the previous models, although not radically different in general appearance.



# ------------------------------------------------------------------------------
# for reference
# ------------------------------------------------------------------------------


( Xp <- predict(gamgr, newdata = mackp, type = "lpmatrix") )

attributes(Xp)$dimnames[[2]]



# ----------
br <- rmvn(n = 1000, coef(gamgr), vcov(gamgr, unconditional = TRUE))



# ----------
mackp[1,]

Xp[1,]

br[1,1:100] %*% Xp[1,1:100]

br[1,] %*% Xp[1,]


br[1,]

Xp[1,]




