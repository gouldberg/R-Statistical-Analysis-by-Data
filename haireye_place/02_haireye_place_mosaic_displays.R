setwd("//media//kswada//MyFiles//R//haireye_place")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyePlace
#   - data on hair color and eye color, for both Caithness and Aberdeen as a 4 * 5 * 2 table
# ------------------------------------------------------------------------------
data("HairEyePlace", package = "vcdExtra")

data <- HairEyePlace

data



# ------------------------------------------------------------------------------
# mosaic display each by place
# ------------------------------------------------------------------------------

mosaic(data[,,"Caithness"], gp = shading_Friendly2)


mosaic(data[,,"Aberdeen"], gp = shading_Friendly2)

