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
# Set coordinates
# ------------------------------------------------------------------------------

dpp <- data.frame(rbind(coordinates(spjpines1), coordinates(spred),  coordinates(spcells)))


head(dpp)



# ----------
njap <- nrow(coordinates(spjpines1))

nred <- nrow(coordinates(spred))

ncells <- nrow(coordinates(spcells))



# ----------
dpp <- cbind(dpp, c(rep("JAPANESE", njap), rep("REDWOOD", nred), rep("CELLS", ncells))) 


names(dpp) <- c("x", "y", "DATASET")


head(dpp)



# ------------------------------------------------------------------------------
# Plot point patterns (spatial distribution) re-scaled to fit in the unit square
# ------------------------------------------------------------------------------

library(lattice)



# Spatial distribution of the location of cell centres, 
# japanese black pine saplings, and saplings of California redwood trees

print(xyplot(y ~ x | DATASET, data=dpp, pch=19, aspect=1))


