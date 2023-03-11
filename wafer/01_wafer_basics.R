setwd("//media//kswada//MyFiles//R//wafer")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wafer
#   - Myers and Montgomery (1997) present data from a step in the manufacturing process for semiconductors.
#     Four factors are believed to influence the resistivity of the wafer and so a full factorial experiment with two levels of each factor was run.
#   - Previous experience led to the expectation that resistivity would have a skewed distribution and so the need for transformation was anticipated.
# ------------------------------------------------------------------------------

data(wafer, package="faraway")

str(wafer)


car::some(wafer)



# ------------------------------------------------------------------------------
# histogram of resist
# ------------------------------------------------------------------------------

summary(wafer)



# ----------
# kurtosis is -0.71
psych::describe(wafer$resist)



# ----------
# Histogram
par(mfrow=c(1,1), mar = c(2,2,2,2))
MASS::truehist(wafer$resist, col = gray(.7))



# ----------
plot(density(wafer$resist))
