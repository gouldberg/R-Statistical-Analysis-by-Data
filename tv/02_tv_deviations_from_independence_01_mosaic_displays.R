setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------

data("TV", package = "vcdExtra")


TV

dim(TV)

str(TV)



# ----------
TV2 <- margin.table(TV, c(1,3))

TV2




# ------------------------------------------------------------------------------
# mosaic display:  Two-way tables
# ------------------------------------------------------------------------------

library(vcd)


mosaic(TV2, gp = shading_Friendly2, labeling = labeling_residuals)




# ----------

margin.table(TV, c(1,2))


mosaic(margin.table(TV, c(1,2)), gp = shading_Friendly2, labeling = labeling_residuals)




# ------------------------------------------------------------------------------
# mosaic display:  3-way tables
# ------------------------------------------------------------------------------


TV.df <- as.data.frame.table(TV)



# ----------
# Time to be only hour
levels(TV.df$Time) <- rep(c("8", "9", "10"), c(4, 4, 3))


TV3 <- xtabs(Freq ~ Day + Time + Network, TV.df)


TV3


pairs(TV3, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))
