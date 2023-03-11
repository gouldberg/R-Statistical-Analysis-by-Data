setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by pairs
# show all bivariate marginal relations
#
#   - mosaic display:  "mosaic.pars"
# ------------------------------------------------------------------------------

# only categorical variable

largs <- list(labeling = labeling_border(varnames = FALSE, labels = c(T, T, F, T), alternalte_labels = FALSE))

dargs <- list(gp_varnames = gpar(fontsize = 20), offset_varnames = -1, labeling = labeling_border(alternat_labels = FALSE))

graphics.off()



# gpairs does not accept formula, but pais accept formula
formula <- ~ died + cancer + admit + uncons

pairs(xtabs(formula, data = ICU), shade = TRUE, space = 0.25, 
      diag_panel_args = dargs, upper_panel_args = largs, lower_panel_args = largs)




# ------------------------------------------------------------------------------
# Data Exploration:  mosaic displays with deviations from independence
# show all bivariate marginal relations
#
#   - labeling = labeling_residulas
# ------------------------------------------------------------------------------


# Investigate closely more for cancer + admit (intercations)
formula <- ~ cancer + admit

mat_dat <- xtabs(formula, data = ICU)

vcd::mosaic(mat_dat, shade=TRUE, suppress=0, gp_args = list(interpolate = 1:8), labeling=labeling_residuals, gp_text = gpar(fontface=2))



# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by scatterplotMatrix
# show all bivariate marginal relations
#
#   - by-group representation
#   - ellipse:  shows SDs and correation direction, the concentration is contolled by "levels"
#   - outliers:  by "id = list(n = x)"
#   - formula type:  can apply data transformation
# ------------------------------------------------------------------------------
# By default:
#   - diagonal panels:  nonparametric density estimates, using an adaptive-kernel estimator, with a rug-plot
#   - solid line:  marginal linear least-squares fit, ignoring the other variables
#   - central broken line:  nonparametric regression smooth
#   - outer broken lines:  smooths of the conditional variation of the y values givne x in each panel, like running quartiles
#   - size of the ellipse in the vertical and horizontal directions reflects the SDs of the two variables. 
# ------------------------------------------------------------------------------
#   - ellipse = list(levels = 0.5):  to get separate 50% concentration ellipses for the groups
#     If the data in a panel are bivariately normally distributed, then the ellipse encloses approximately 50% of the points
# ------------------------------------------------------------------------------

library(car)

formula <- ~ died + age + cancer + admit + uncons

scatterplotMatrix(formula, data = ICU,
                  smooth = list(span = 0.5), 
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3))


# ----------
# by group

formula <- ~ died + age + cancer + admit + uncons | died

scatterplotMatrix(formula, data = ICU,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black"))



