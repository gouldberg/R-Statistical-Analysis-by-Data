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
# Data Exploration:  Visualize spatial location by climate region
# ------------------------------------------------------------------------------

tmp <- swer %>% dplyr::select(N, E, code, climate.region) %>% unique()

xyplot(N ~ E,
       aspect = "iso", pch = 1:12, col = 1:12, cex = 2,
       data = tmp, 
       groups = tmp$climate.region,
       xlab = "East-coordinate",  ylab = "North-coordinate")



xyplot(N ~ E | climate.region, 
       aspect = "iso", col = 1, cex = 2,
       data = tmp, xlab = "East-coordinate",  ylab = "North-coordinate")



# ----------
# The annual maximum 12 hourly rainfall, exra, by region.
xyplot(N ~ E, data = swer, cex = sqrt(swer$exra) / 5, col = "black")




# ------------------------------------------------------------------------------
# Data Exploration:  Visualize spatial location with bubble plot way
# ------------------------------------------------------------------------------

# library(gstat)
library(sp)


# substract mean, since bubble function plots negative values as first color and positive values as second color
mydata <- data.frame(exra = swer$exra - mean(swer$exra), swer$N, swer$E)

coordinates(mydata) <- c("swer.E", "swer.N")


bubble(mydata, "exra", col = c(gray(0.7), "black"), main = "exra", xlab = "East-coordinate", ylab = "North-coordinate")



# -->
# Alpine Sourth Side has large annual maximum 12 hourly rainfall

