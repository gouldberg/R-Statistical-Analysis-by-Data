# setwd("//media//kswada//MyFiles//R//mite")
setwd("//media//kswada//MyFiles//R//Spatial_data_analysis//mite")

packages <- c("dplyr", "ape", "spdep", "ade4", "adegraphics", "adespatial", "vegan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mite
#  - Substrate (7 classes),  Shrubs (3 classes), and Microtopography (2 classes)
# ------------------------------------------------------------------------------

load("./data/mite.RData")


dim(mite)
car::some(mite)


dim(mite.env)
car::some(mite.env)


dim(mite.xy)
car::some(mite.xy)



source("./functions/plot.links.R")
source("./functions/sr.value.R")
source("./functions/quickMEM.R")
source("./functions/scalog.R")



# ------------------------------------------------------------------------------
# RDA with 3rd-degree polynomial (non-orthogonal) of geographical coordinates
# ------------------------------------------------------------------------------

# compute raw (non-orthogonal) 3rd-degree polynomical
mite.poly <- poly(as.matrix(mite.xy.c), degree = 3, raw = TRUE)


colnames(mite.poly) <- c("X", "X2", "X3", "Y", "XY", "X2Y", "Y2", "XY2", "Y3")



# ----------
# RDA with all 9 polynomial terms
( mite.trend.rda <- rda(mite.h ~ ., data = as.data.frame(mite.poly)) )



# ----------
# Computation of the adjusted R^2
( R2adj.poly <- RsquareAdj(mite.trend.rda)$adj.r.squared )




# ------------------------------------------------------------------------------
# RDA using a third-degree orthogonal polynomial of the geographic coordinates
# ------------------------------------------------------------------------------

mite.poly.ortho <- poly(as.matrix(mite.xy), degree = 3)


colnames(mite.poly.ortho) <-  c("X", "X2", "X3", "Y", "XY", "X2Y", "Y2", "XY2", "Y3")



# ----------
( mite.trend.rda.ortho <- rda(mite.h ~ ., data = as.data.frame(mite.poly.ortho)))



# ----------
( R2adj.poly2 <- RsquareAdj(mite.trend.rda.ortho)$adj.r.squared )




# ------------------------------------------------------------------------------
# Forward selection using Blanchet et al. (2008a) double stopping criterion
# ------------------------------------------------------------------------------

( mite.trend.fwd <- adespatial::forward.sel(mite.h, mite.poly.ortho, adjR2thresh = R2adj.poly2) )



mite.trend.fwd



# -->
# New RDA using the 6 terms retained



# ------------------------------------------------------------------------------
# parsimonious RDA
# ------------------------------------------------------------------------------

( mite.trend.rda2 <- rda(mite.h ~ ., data = as.data.frame(mite.poly)[ ,mite.trend.fwd[ ,2]]) )


mite.trend.rda2




# ------------------------------------------------------------------------------
# Test the significance of terms
# ------------------------------------------------------------------------------

# global test
anova(mite.trend.rda2)



# test of all canonical axes
anova(mite.trend.rda2, by = "axis")



# -->
# only axes 1 - 3 are significant



# ------------------------------------------------------------------------------
# Plot of the three independent significant spatial structures (canonical axes) plus the fourth (p-value around 0.06). 
# ------------------------------------------------------------------------------

# Note that the fitted scores in scaling 1 is used here.
# We want to display the "pure" spatial model, i.e., the linear combination of spational variables, 
# in a projection preserving the Euclidean distances among sites.

mite.trend.fit <- scores(mite.trend.rda2, choices = 1:4, display = "lc", scaling = 1)


head(mite.trend.fit)



# ----------
s.value(mite.xy, mite.trend.fit, symbol = "circle")



# -->
# Other plotting code with homemade function sr.value:
# dev.new(title = "Mite Trend Surface Analysis")
# par(mfrow = c(1,3))
# sr.value(mite.xy,mite.trend.fit[ ,1])
# sr.value(mite.xy,mite.trend.fit[ ,2])
# sr.value(mite.xy,mite.trend.fit[ ,3])


# -->
# This analysis shows that the oribatid mite community is significantly spatially structured,
# and that three (or four, depending on the run) significant independent models can be obtained.

# The first one (forst canonical axis, 73.8% of the explained variance) displays a strong difference between the upper and the lower half of the area.
# The next two significant models (12.1% and 8.4% of the explained variance ,respectively) display finer-scaled structures.
# The fourth axis (only 3.0% variance, p is close to 0.05 depending on the run) shows a left-right contrast


