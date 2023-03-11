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
# classical test of independence:  [Hair][Eye]
# ------------------------------------------------------------------------------
mod1 <- MASS::loglm(~ Hair + Eye, data = data)


mod1


residuals(mod1)


mosaic(data, expected = ~ Hair + Eye, gp = shading_Friendly2)

