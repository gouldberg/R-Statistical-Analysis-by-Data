setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "RColorBrewer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ----------
# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
latlong <- latlong[-8,]



# ------------------------------------------------------------------------------
# Post-hoc curve fitting of environmental variables
#  - Examine how selected environmental variables are connected to the ordination result, but on a broader, non-linear basis.
#  - vegan::ordisurf() fits smoothed two-dimensional splines by means of GAM (generalized additive models)
# ------------------------------------------------------------------------------

graphics.off();  par(mfrow=c(1,1));

plot(spe.ca, main = "CA fish abundances - scaling 2", sub = "Fitted curves: discharge (red), ammonium (green)")

spe.ca.env <- envfit(spe.ca ~ dis + amm, env)

plot(spe.ca.env)  # Two arrows

vegan::ordisurf(spe.ca, env$dis, add = TRUE)

vegan::ordisurf(spe.ca, env$amm, add = TRUE, col = "green")



# -->
# water discharge (dis) fitted surface is strongly nonlinear,
# whereas the ammonium concentration (amm) fitted surface is made of straight, parallel lines, indicating a linear fit.
