setwd("//media//kswada//MyFiles//R//criminal")

packages <- c("dplyr", "vcd", "vcdExtra", "logmult", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  criminal
# ------------------------------------------------------------------------------
data("criminal", package = "logmult")

data <- criminal

data



# ------------------------------------------------------------------------------
# test of independence between Year and Age
# ------------------------------------------------------------------------------
mod1 <- MASS::loglm(~ Year + Age, data = data)



# Lack of fit for independece model,
# indicating that dropping of charges in relation to age changed over tye years recorded here.
mod1


residuals(mod1, type = "pearson")




# ------------------------------------------------------------------------------
# display the pattern of signs and magnitudes of the residuals
# ------------------------------------------------------------------------------
mosaic(data, expected = ~ Year + Age, shade = TRUE, labeling = labeling_residuals)

mosaic(data, expecgted = ~ Year + Age, shade = TRUE, labeling = labeling_residuals, gp=shading_Friendly, interpolate = 1:4)





