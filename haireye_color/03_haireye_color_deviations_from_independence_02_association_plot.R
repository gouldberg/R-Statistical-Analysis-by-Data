setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data



# ------------------------------------------------------------------------------
# Association plot
#  - puts deviation from independence in the foreground: the area of each box is made proportional to the (observed - expected) frequency
#
#  - For a 2-way contingency table, 
#    the signed contribution to Pearson X^2 for cell i, j (r(ij): Pearson residual) = { n(ij) - m(ij) } / sqrt(m(ij)),  X^2 = sum(r(ij)^2)
#    n(ij):  observed cell frequency   m(ij):  expected cell frequency
#    In the association plot, each cell is shown by a rectangle, having
#      - (signed) height ~ r(ij)    width = sqrt(m(ij))
#
#  - Each rectangle is shaded according to the value of the Pearson residual using a simple scale, where residuals |r(ij)| > 2 are shaded blue or red depending on their sign,
#    and residuals |r(ij)| > 4 are shaded with a more saturated color.
# ------------------------------------------------------------------------------
# association plot of marginal table, collapsed over gender
assoc(~ Hair + Eye, data = data, shade = TRUE)



# association plot of full table
assoc(data, shade = TRUE)



