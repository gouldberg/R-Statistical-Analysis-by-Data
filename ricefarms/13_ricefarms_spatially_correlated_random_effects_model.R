setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# data:  riceww
#  - contiguity matrix where for each farm all other farms from the same village are defined as neighbors
# ------------------------------------------------------------------------------

data("riceww", package = "splm")


str(riceww)

dim(riceww)




# ------------------------------------------------------------------------------
# Spatial Correlated Random Effects model
#   - Spatial correlation applies to both the individual effects and the remainder error components.
#     Although the two data-generating processes look similar, they do imply different spatial spillover mechanisms governed
#     by a different structure of the implied variance-covariance matrix/
# ------------------------------------------------------------------------------


# SEMRE:  only the time-varying components diffuse spatially
semremod.ml <- spml(riceprod0, Rice, listw = ricelw, model = "random", lag = FALSE, spatial.error = "b")


summary(semremod.ml)



# ----------
# SEM2RE:  spatial spillovers too have a permanent component
sem2remod.ml <- spml(riceprod0, Rice, listw = ricelw, model = "random", lag = FALSE, spatial.error = "kkp")


summary(sem2remod.ml)




# -->
# The differences are minimal.
# Random effects are significant, albeith weak in magnitude, while in accordance with the original work of Druska and Horrace (2004),
# very strong spatial error correlation is detected.

# The limited importance of the RE component makes the distinction between the two specifications scarcely relevant.






