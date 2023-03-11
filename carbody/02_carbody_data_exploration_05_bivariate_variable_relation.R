
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\carbody")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  car body coating process data
# ------------------------------------------------------------------------------

dat <- read.csv("carbody_dat.txt", header = TRUE, skip = 2, sep = "\t")


str(dat)


car::some(dat)





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

formula <- ~ kishaku + nendo + gunspeed + fukituke + airpres + patternwid + hakidashi + toryotemp + temp + moisture + tochaku


scatterplotMatrix(formula, data = dat,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)





# ----------

psych::pairs.panels(dat, stars = TRUE, method = "spearman")


