setwd("//media//kswada//MyFiles//R//meuse")

packages <- c("dplyr", "maptools", "spdep", "RColorBrewer", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


pal = function(n = 9) brewer.pal(n, "Reds")



# ------------------------------------------------------------------------------
# data:  meuse
# ------------------------------------------------------------------------------
library(sp)

data(meuse)
data(meuse.grid)


# ----------
coordinates(meuse) <- c("x", "y")
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")


# ----------
head(meuse)
head(meuse.grid)



# ------------------------------------------------------------------------------
# simple linear regression by lm()
# ------------------------------------------------------------------------------
zn.lm <- lm(log(zinc) ~ sqrt(dist), meuse)

summary(zn.lm)


meuse.grid$pred <- predict(zn.lm, meuse.grid)

meuse.grid$se.fit <- predict(zn.lm, meuse.grid, se.fit=TRUE)$se.fit



# ------------------------------------------------------------------------------
# simple linear regression by krige()
# ------------------------------------------------------------------------------

# In this case, doesnot krige as no variogram is specified, but uses linear regression.
# Used in this form, the result is identical to that of lm.

# However, it can also be used to predict with regression models that are refitted withinlocal neighbourhoods around a prediction location
# or provide mean predicted values for spatial areas.  The variance it returns is the prediction error variance
# when predicting for points or the estimation error variance when used for blocks.

meuse.lm <- krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid)


summary(meuse.lm)
summary(zn.lm)



# ------------------------------------------------------------------------------
# second-order polynomial regression by krige():  trend surface analysis
# ------------------------------------------------------------------------------
meuse.tr2 <- krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 2)

summary(meuse.tr2)



# ----------
# Also we can use lm() for trend surface analysis for the second-order trend with a formula using I to treat powers and products 'as-is'
# but lm() does not standardise coordinates, which often yields huge numbers when powered.
lm(log(zinc) ~ I(x^2) + I(y^2) + I(x*y) + x + y, meuse)



# The second form does standardise coordinates in such a way that it cannot be used in a subsequent predict call with different coordinate ranges.
lm(log(zinc) ~ poly(x, y, degree = 2), meuse)




