setwd("//media//kswada//MyFiles//R//rugged")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rugged
# ------------------------------------------------------------------------------

data("rugged", package = "rethinking")

d <- rugged

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# Data Exploration:  Visualize spatial location
# ------------------------------------------------------------------------------

xyplot(lat ~ lon | cont_africa, aspect = "iso", col = 1, 
       data = d, xlab = "lon",  ylab = "lat")



xyplot(lat ~ lon, 
       aspect = "iso", 
       groups = d$cont_africa,
       data = d, xlab = "lon",  ylab = "lat", col = 1:2, pch = c(1,20), cex = 0.8)



# ------------------------------------------------------------------------------
# Data Exploration:  Visualize spatial location with bubble plot way
# ------------------------------------------------------------------------------

# library(gstat)
library(sp)


# substract mean, since bubble function plots negative values as first color and positive values as second color
mydata <- data.frame(rgdppc_2000 = d$rgdppc_2000 - mean(d$rgdppc_2000), d$lon, d$lat)

coordinates(mydata) <- c("d.lon", "d.lat")


bubble(mydata, "rgdppc_2000", col = c(gray(0.7), "black"), main = "rgdppc_2000", xlab = "lon", ylab = "lat")



# ----------
mydata <- data.frame(rugged = d$rugged - mean(d$rugged), d$lon, d$lat)

coordinates(mydata) <- c("d.lon", "d.lat")


bubble(mydata, "rugged", col = c(gray(0.7), "black"), main = "rugged", xlab = "lon", ylab = "lat")



# -->
# Seychelles is something different from other African countries


