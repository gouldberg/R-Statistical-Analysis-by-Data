# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\silvia")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  silvia
# ------------------------------------------------------------------------------

dat <- read.csv("silvia_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)



# ----------
dat$TM <- as.factor(dat$TM)


dat$DEAL <- as.factor(dat$DEAL)



# ----------
# price down
dat <- dat %>% mutate(pd = P - NP)


# KM == 0
dat <- dat %>% mutate(km_z = KM == 0)




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

formula <- ~ KM + Yr + CT + pd


scatterplotMatrix(formula, data = dat,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)



# ----------
# by TM

scatterplotMatrix(formula, data = dat, groups = dat$TM,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black"), pch = c(1, 20))


# by TM without km_z

tmp <- dat %>% filter(km_z == FALSE)

scatterplotMatrix(formula, data = tmp, groups = tmp$TM,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black"), pch = c(1, 20))



# -->
# KM * TM interaction is suggested




# ----------
# by DEAL

scatterplotMatrix(formula, data = dat, groups = dat$DEAL,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black"), pch = c(1, 20))


# by DEAL without km_z

tmp <- dat %>% filter(km_z == FALSE)

scatterplotMatrix(formula, data = tmp, groups = tmp$DEAL,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black"), pch = c(1, 20))



