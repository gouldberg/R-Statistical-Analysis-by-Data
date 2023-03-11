setwd("//media//kswada//MyFiles//R//debt")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  debt
# ------------------------------------------------------------------------------

data("debt", package = "faraway")


str(debt)


head(debt)




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


# by group

formula <- ~ incomegp + house + children + agegp + manage + locintrn | ccarduse


scatterplotMatrix(formula, data = debt,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.8), gray(0.4), "black"), pch = 1:20)




# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by psych::describe
# show all bivariate marginal relations
#
#   - stars:  shows the significance of correlations
# ------------------------------------------------------------------------------

library(psych)


var_obj <- c("incomegp", "house", "children", "agegp", "manage", "ccarduse", "locintrn", "prodebt")


# here we apply method = "spearman" due to terrible skewness
pairs.panels(debt[,var_obj], ci = TRUE, smoother = TRUE, stars = TRUE, method = "spearman")


