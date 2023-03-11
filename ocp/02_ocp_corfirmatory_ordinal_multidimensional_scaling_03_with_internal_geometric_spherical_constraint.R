setwd("//media//kswada//MyFiles//R//ocp")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  OCP
# ------------------------------------------------------------------------------

data("OCP", package = "smacof")

str(OCP)




# ------------------------------------------------------------------------------
# MDS with internal constraints:  spherical SMACOF
# ------------------------------------------------------------------------------

# The plot of the solution of unrestricted MDS is that
# this plot suggests that many items are aligned approximately on a circle.
# Some points deviate from this circular structure and are shifted toward the center.

# Using a spherical MDS, we can force all the points to be on a circle.
# The distances between the objects can be interpreted by means of the distances on the circumference (i.e., geodesic distances)

fit.ocp3 <- smacofSphere(ocpD, penalty = 1000, type = "ordinal")

fit.ocp3



# ----------
colpal <- c(rainbow_hcl(4, c = 100, l = 40), col2hex("lightgray"))
pal <- palette(colpal)
op <- par(mar=c(5.1, 4.1, 4.1, 10.1), xpd = TRUE)
plot(fit.ocp3, col = cols, label.conf = list(col = cols))
legend("right", legend = levels(OCP[,55]), inset=c(-0.48,0), col = colpal, pch = 19, title = "Facets")
par(op)
palette(pal)



# -->
# The resulting stress value is 0.262 which is barely worse than the unrestricted stress.
# Items deviating from the circular structure (i35, i36) deserve closer attention.



# ----------
# Shepard Diagram  --> Can not show Shepard diagram for Spherical Constraint solution.
par(mfrow = c(1,3))
plot(fit.ocp1, plot.type = "Shepard", main = "Shepard Diagram: Unrestricted")
plot(fit.ocp2, plot.type = "Shepard", main = "Shepard Diagram: Regional Constraint")
plot(fit.ocp3, plot.type = "Shepard", main = "Shepard Diagram: Spherical Constraint")

