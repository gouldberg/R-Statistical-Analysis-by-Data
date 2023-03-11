setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "RgoogleMaps", "googleVis", "gclus")
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
? vegan::decostand



# ------------------------------------------------------------------------------
# Ecological Data Transformation
#   - Chord transformation:  give a length (norm) of 1 to each row vector
# ------------------------------------------------------------------------------

# The chord transformation is useful prior to PCA and RDA, and k-means partitioning, and can also be applied to log-transformed data

spe.norm <- decostand(spe, "normalize")  # default MARGIN = 1

round(spe.norm[1:5, 2:4], 3)

round(spe.rel[1:5, 2:4], 3)

vec.norm <- function(x) sqrt(sum(x^2))

apply(spe.norm, 1, vec.norm)



# ------------------------------------------------------------------------------
# Ecological Data Transformation
#   - Hellinger transformation
# ------------------------------------------------------------------------------

# The Hellinger transformation is useful prior to PCA and RDA, and k-means partitioning
# The Hellinger transformation can also be obtained by applying the chord transformation to square-root-transformed species data

spe.hel <- decostand(spe, "hellinger")

apply(spe.hel, 1, vec.norm)


round(spe.rel[1:5, 2:4], 3)

round(spe.norm[1:5, 2:4], 3)

round(spe.hel[1:5, 2:4], 3)



