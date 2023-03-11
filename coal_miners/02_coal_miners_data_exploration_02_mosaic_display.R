setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# mosaic display
# ------------------------------------------------------------------------------


dat2_2 <- margin.table(dat2, c(1,2))


mosaic(dat2_2, shade = TRUE, gp_args = list(interpolate = 1:4), labeling = labeling_residuals)



# ----------
interp <- function(x) pmin(x / 6, 1)

mosaic(dat2_2, shade = TRUE, gp_args = list(interpolate = interp))




# ------------------------------------------------------------------------------
# mosaic display:  non-parametric independence test
#
#   - shading_max:  generate n = 1000 random tables with the same margins, and computes the maximum residual statistic for each.
#     This gives a non-parametric p-value for the test of independence, shown in the legend
#     This calls coindep_test() in background
# ------------------------------------------------------------------------------

mosaic(dat2_2, gp = shading_max, labeling = labeling_residuals)




# ------------------------------------------------------------------------------
# mosaic display:  multi-way
# ------------------------------------------------------------------------------

mosaic(dat2, shade = TRUE, gp_args = list(interpolate = 1:4), labeling = labeling_residuals)




