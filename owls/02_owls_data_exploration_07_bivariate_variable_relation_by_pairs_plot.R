# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



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

formula <- ~ log10(NegPerChick + 0.1) + ArrivalTime


scatterplotMatrix(formula, data = Owls,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)



# ----------
# by group

formula <- ~ log10(NegPerChick + 0.1) + ArrivalTime | SexParent

formula <- ~ log10(NegPerChick + 0.1) + ArrivalTime | FoodTreatment


scatterplotMatrix(formula, data = Owls,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c("black", gray(0.5), gray(0.7)), pch = c(1,2,20))




# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by psych::describe
# show all bivariate marginal relations
#
#   - stars:  shows the significance of correlations
# ------------------------------------------------------------------------------

library(psych)


var_obj <- c("NegPerChick", "ArrivalTime", "SexParent", "FoodTreatment")


# here we apply method = "spearman" due to terrible skewness
pairs.panels(Owls[, var_obj], ci = TRUE, smoother = TRUE, stars = TRUE, method = "spearman")


