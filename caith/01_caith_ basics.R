setwd("//media//kswada//MyFiles//R//caith")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  caith
#  - Another classic 4 * 5 table tabulating hair color and eye color.
#    This for people in Caithness, Scotland, originally from Fisher (1940).
# ------------------------------------------------------------------------------
data("caith", package = "MASS")

data <- caith


# ----------
# This is data frame
data
str(data)



# ----------
data <- as.matrix(data)
names(dimnames(data)) <- c("Eye", "Hair")



# ------------------------------------------------------------------------------
# mosaic display
# ------------------------------------------------------------------------------
mosaic(data, gp = shading_Friendly2)


