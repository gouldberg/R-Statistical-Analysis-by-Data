
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\odakyu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  odakyu
# ------------------------------------------------------------------------------

dat <- read.csv("odakyu_dat.txt", header = TRUE, sep = "\t")


str(dat)


car::some(dat)



# ----------
dat$floor <- as.factor(dat$floor)

dat$ac <- as.factor(dat$ac)

dat$closet <- as.factor(dat$closet)

dat$flooring <- as.factor(dat$flooring)

dat$balcony <- as.factor(dat$balcony)



# ----------
# total minutes
dat$min_tot <- dat$bus_min + dat$walk_min + dat$train_min


# price including train fares for 20 days per month
dat$price_tot <- dat$price + dat$train_fare * 20


# price per area
dat$price_ave <- dat$price / dat$area
dat$price_tot_ave <- dat$price_tot / dat$area


# bus
dat <- dat %>% mutate(bus_flg = ifelse(bus_min > 0, 1, 0))
dat$bus_flg <- as.factor(dat$bus_flg)



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

formula <- ~ years + min_tot + km + shikirei + price_ave


scatterplotMatrix(formula, data = dat,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)




