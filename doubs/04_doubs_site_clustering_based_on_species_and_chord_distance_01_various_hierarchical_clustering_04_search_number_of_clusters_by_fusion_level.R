setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "RColorBrewer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ----------
# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
latlong <- latlong[-8,]



# ------------------------------------------------------------------------------
# Looking for interpretable clusters:  Fusion level values
# ------------------------------------------------------------------------------

# Plotting the fusion level values may help define cutting levels.

# Reading from right to left, long horizontal lines preceding steep increases suggest cutting levels,
# the 9-group in the complete linkage and 4-group in the Ward.D2

graphics.off()
par(mfrow = c(2,2))

plot(spe.ch.complete$height, nrow(spe):2, type = "S", main = "Fusion levels - Chord - Complete", ylab = "k (number of clusters)", xlab = "h (node height)", col = "grey")
text(spe.ch.complete$height, nrow(spe):2, nrow(spe):2, col = "red", cex = 0.8)

plot(spe.ch.UPGMA$height, nrow(spe):2, type = "S", main = "Fusion levels - Chord - UPGMA", ylab = "k (number of clusters)", xlab = "h (node height)", col = "grey")
text(spe.ch.UPGMA$height, nrow(spe):2, nrow(spe):2, col = "red", cex = 0.8)

plot(spe.ch.ward$height, nrow(spe):2, type = "S", main = "Fusion levels - Chord - ward.D2", ylab = "k (number of clusters)", xlab = "h (node height)", col = "grey")
text(spe.ch.ward$height, nrow(spe):2, nrow(spe):2, col = "red", cex = 0.8)

plot(spe.ch.beta2$height, nrow(spe):2, type = "S", main = "Fusion levels - Chord - Beta-flexible", ylab = "k (number of clusters)", xlab = "h (node height)", col = "grey")
text(spe.ch.beta2$height, nrow(spe):2, nrow(spe):2, col = "red", cex = 0.8)

plot(spe.ch.single$height, nrow(spe):2, type = "S", main = "Fusion levels - Chord - Single", ylab = "k (number of clusters)", xlab = "h (node height)", col = "grey")
text(spe.ch.single$height, nrow(spe):2, nrow(spe):2, col = "red", cex = 0.8)



# ------------------------------------------------------------------------------
# Cutree
# ------------------------------------------------------------------------------

# Number of groups 4 where at least a small jump is present in all four graphs of fusion level

k <- 4

spech.single.g <- cutree(spe.ch.single, k = k)

spech.complete.g <- cutree(spe.ch.complete, k = k)

spech.UPGMA.g <- cutree(spe.ch.UPGMA, k = k)

spech.ward.g <- cutree(spe.ch.ward, k = k)

spech.beta2.g <- cutree(spe.ch.beta2, k = k)


