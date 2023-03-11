setwd("//media//kswada//MyFiles//R//japanesepines")

packages <- c("dplyr", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  japanesepines
# ------------------------------------------------------------------------------

data("japanesepines", package = "spatstat")

class(japanesepines)

str(japanesepines)



# ----------
# convert to Spatial Points class
spjpines <- as(japanesepines, "SpatialPoints")

str(spjpines)

# Convert to unit square using the elide methods
spjpines1 <- elide(spjpines, scale=TRUE, unitsq=TRUE)



# ----------
data(redwoodfull, package = "spatstat")

class(redwoodfull)

spred <- as(redwoodfull, "SpatialPoints")



# ----------
data(cells, package = "spatstat")

class(cells)

spcells <- as(cells, "SpatialPoints")



# ----------
summary(spjpines1)

summary(spred)

summary(spcells)



# ------------------------------------------------------------------------------
# Kernel density
# ------------------------------------------------------------------------------

# When calling density on a ppp object (which in fact calls density.ppp), we have used the additional arguments dimxy and xy to make sure that the grid
# used to compute the estimates is compatible with that stored in kernels.
# Finally, the kernel estimate is returned in an im class that is converted into a SpatialGridDataFrame and the values incorporated into kernels.

cc <- coordinates(kernels)

xy <- list(x=cc[,1], y=cc[,2])


head(xy)



# ----------
k4 <- density(as(spred, "ppp"), .5*bw, dimyx=c(100, 100), xy=xy)

kernels$k4 <- as(k4, "SpatialGridDataFrame")$v


k5 <- density(as(spred, "ppp"), .5*.05, dimyx=c(100, 100), xy=xy)

kernels$k5 <- as(k5, "SpatialGridDataFrame")$v


k6 <- density(as(spred, "ppp"), .5*.1, dimyx=c(100, 100), xy=xy)

kernels$k6 <- as(k6, "SpatialGridDataFrame")$v


k7 <- density(as(spred, "ppp"), .5*.15, dimyx=c(100, 100), xy=xy)

kernels$k7 <- as(k7, "SpatialGridDataFrame")$v


summary(kernels)



# ------------------------------------------------------------------------------
# plot kernel density of redwood point pattern
# ------------------------------------------------------------------------------

library(RColorBrewer)

gp <- brewer.pal(8, "Reds")

print(spplot(kernels, at=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), col.regions=colorRampPalette(gp)(15)[1:12], 
             names.attr=c(paste("Q bw=",round(bw, digits=4), sep="", collapse=""),
                          "Q bw=0.05", "Q bw=0.1","Q bw=0.15", paste("G bw=", round(.5*bw, digits=4),
                                                                     sep="", collapse=""), "G bw=0.025", "G bw=0.05","G bw=0.075"), cex=0.7, colorkey=FALSE))

