# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ------------------------------------------------------------------------------
# Data Exploration
# ------------------------------------------------------------------------------

summary(RIKZ)


# -->
# Note that there some zero values in "Richness" (dependent variable)



# ----------
xtabs(~ Beach, data = RIKZ)


xtabs(~ Beach + Exposure, data = RIKZ)



# -->
# Exposure = 8 is only for Beach 2
