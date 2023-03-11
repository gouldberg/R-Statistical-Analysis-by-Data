# setwd("//media//kswada//MyFiles//R//boreality")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//boreality")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  boreality
# ------------------------------------------------------------------------------

Boreality <- read.table(file = "Boreality.txt", header = TRUE)


str(Boreality)


dim(Boreality)


car::some(Boreality)



# ----------
# Boreality was transformed using the following transformation (See Cressie (p.395, 1993))
# nBor:  the number of species that belong to breal coenosis species
# nTot:  number of all species at the site
Boreality$Bor <- sqrt(1000 * (Boreality$nBor + 1) / (Boreality$nTot))




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

formula <- ~ Bor + Wet + NDVI + Grn + Oxalis + T61


scatterplotMatrix(formula, data = Boreality,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)




# ----------
formula <- ~ Bor + Wet + NDVI + Grn + T61


scatterplotMatrix(formula, data = Boreality, groups = Boreality$Oxalis,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c(gray(0.7), "black"), pch = c(1,20))



# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by psych::describe
# show all bivariate marginal relations
#
#   - stars:  shows the significance of correlations
# ------------------------------------------------------------------------------

library(psych)


var_obj <- c("Bor", "Wet", "NDVI", "Grn", "T61")


# here we apply method = "spearman" due to terrible skewness
pairs.panels(Boreality[, var_obj], ci = TRUE, smoother = TRUE, stars = TRUE, method = "spearman")



# -->
# Many variables are highly correlated.


