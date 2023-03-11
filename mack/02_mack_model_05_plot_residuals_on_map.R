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
# residuals on map
# ------------------------------------------------------------------------------

res <- mack$egg.count - predict(gm2, mack)

mack2 <- mack %>% mutate(res = res, res_sign = ifelse(res > 0, 1, 0))


graphics.off()

par(mfrow = c(1,1))
plot(coast$lon, coast$lat, type="l", col = "blue", las = 1)

par(new = T)
plot(lat ~ lon, groups = res_sign, cex = sqrt(res^2)/50, col = c("black", gray(0.7)), data = mack2, xlab = "", ylab = "", xaxt = "n", yaxt = "n")




# ----------
# library(gstat)
library(sp)


mydata <- data.frame(res = res, mack$lat, mack$lon)

coordinates(mydata) <- c("mack.lon", "mack.lat")

bubble(mydata, "res", col = c(gray(0.8), gray(0.2)), main = "residuals", xlab = "Longitude", ylab = "Latitude")

plot(coast$lon, coast$lat, type="l", col = "blue", las = 1)



