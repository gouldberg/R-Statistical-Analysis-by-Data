setwd("//media//kswada//MyFiles//R//bartlett")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bartlett data on plum-root cuttings
# ------------------------------------------------------------------------------
data("Bartlett", package = "vcdExtra")

data <- Bartlett

data



# ------------------------------------------------------------------------------
# 3D mosaic plot
#   - The vcdExtra package implements 3D mosaics using rgl graphics.
#   - mosaic3d() provides methods for "loglm" as well as "table" (or "structable") objects.
# ------------------------------------------------------------------------------

mosaic3d(data)



# --> The cuttings are more likely to be alive when planted Now and when cut Long.
# These relations can more easily be appreciated by rotating the 3D display.
