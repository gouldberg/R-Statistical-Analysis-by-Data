setwd("//media//kswada//MyFiles//R//pashkam")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Pashkam
# ------------------------------------------------------------------------------

data("Pashkam", package = "MPsychoR")

str(Pashkam)


Pashkam



# ------------------------------------------------------------------------------
# Multidimensional Scaling by each category
# ------------------------------------------------------------------------------

# We focus on the V1 (primary visual cortex) activation in the "color on objects and background" condition.

# Fit an interval MDS on each of the two dissimilarity matrices separately.
library(smacof)

fitcolor <- mds(Pashkam$color, type = "interval")

fitshape <- mds(Pashkam$shape, type = "interval")

fitcolor

fitshape



# ----------
par(mfrow = c(1, 2))
plot(fitcolor, main = "color")
plot(fitshape, main = "shape")



# ------------------------------------------------------------------------------
# Procrustes
#   - Procrutes does not change the fit of an MDS model
#     It only performs rotations, dilations, and translations of the configuration
# ------------------------------------------------------------------------------

# X:  target configuration
# Y:  testee configuration subject to three possible transformations: rotation, dilation, transration.
fitproc <- Procrustes(X = fitcolor$conf, Y = fitshape$conf)

fitproc


# -->
# We get a congruence coefficient of 0.99 which suggests that both conigurations are highly similar.


# ----------
op <- par(mfrow = c(1,2))
plot(fitcolor, col = "cadetblue", label.conf = list(col = "cadetblue"), main = "Separate MDS Configurations",  ylim = c(-1, 1))
points(fitshape$conf, col = "coral1", pch = 20)
text(fitshape$conf, labels = rownames(fitshape$conf), col = "coral1", cex = 0.8, pos = 3)
legend("bottomright", col = c("cadetblue", "coral"), legend = c("Color Task", "Shape Task"), pch = 19)
plot(fitproc, main = "Procrustes Configuration", ylim = c(-1, 1))
legend("bottomright", col = c("cadetblue", "coral"), legend = c("Color Task", "Shape Task"), pch = 19)
par(op)



