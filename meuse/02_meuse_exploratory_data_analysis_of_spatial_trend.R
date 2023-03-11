setwd("//media//kswada//MyFiles//R//meuse")

packages <- c("dplyr", "maptools", "spdep", "RColorBrewer", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


pal = function(n = 9) brewer.pal(n, "Reds")


# ------------------------------------------------------------------------------
# data:  meuse
# ------------------------------------------------------------------------------
library(sp)

data(meuse)



# ----------
# This is data frame
str(meuse)

car::some(meuse)



# ------------------------------------------------------------------------------
# Check the spatial trend:  relation between top soil zinc concentration and distance to th eriver
# ------------------------------------------------------------------------------
# convert to SpatialPointDataFrame
coordinates(meuse) <- c("x", "y")

str(meuse)



# ----------
# scatter plot
print(xyplot(log(zinc) ~ sqrt(dist), as.data.frame(meuse), asp = .8), split = c(1,1,2,1), more = TRUE)



# ----------
zn.lm <- lm(log(zinc) ~ sqrt(dist), meuse)

meuse$fitted.s <- predict(zn.lm, meuse) - mean(predict(zn.lm, meuse))

meuse$residuals <- residuals(zn.lm)

print(spplot(meuse, c("fitted.s", "residuals"), col.regions = pal(), cuts = 8, colorkey=TRUE), split = c(2,1,2,1))



# -->
# Although the trend removes a large part of the variability, the residuals do not appear to behave as spatially unstructured or white noise:
# residuals witha similar value occur regularly close to another.

