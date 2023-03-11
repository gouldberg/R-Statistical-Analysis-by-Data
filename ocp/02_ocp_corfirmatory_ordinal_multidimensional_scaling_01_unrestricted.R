setwd("//media//kswada//MyFiles//R//ocp")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  OCP
# ------------------------------------------------------------------------------

data("OCP", package = "smacof")

str(OCP)




# ------------------------------------------------------------------------------
# Unrestricted ordinal 2D MDS
# ------------------------------------------------------------------------------

library(smacof)



# ----------
# convert the similarities into dissimilarities
ocpD <- sim2diss(OCP[, 1:54], method = "corr")

ocpD



# ----------
# fit an unrestricted, ordinal 2D MDS solution
fit.ocp1 <- mds(ocpD, type = "ordinal")


fit.ocp1



library("colorspace")
library("gplots")
cols <- as.numeric(OCP[,55])   ## colors
colpal <- c(rainbow_hcl(4, c = 100, l = 40), col2hex("lightgray"))
pal <- palette(colpal)
op <- par(mar = c(5.1, 4.1, 4.1, 10.1), xpd = TRUE)
plot(fit.ocp1, col = cols, label.conf = list(col = cols), main = "OCP Configuration (Unrestricted)")
legend("right", legend = levels(OCP[,55]), inset = c(-0.48,0), col = colpal, pch = 19, title = "Classes")
par(op)



# -->
# The stress value for this unrestricted solution is 0.233.
# For the classified items, we see that each group inhabits a certain region of the MDS space with a few exceptions:
# in the "openness to change" class, items 10 and 46 are far off from the other items belonging to this group;
# in the "delf-enhancement" class, MDS moved items 35 and 36 away from their friends.



# ----------
# Shepard Diagram
plot(fit.ocp1, plot.type = "Shepard", main = "Shepard Diagram")

