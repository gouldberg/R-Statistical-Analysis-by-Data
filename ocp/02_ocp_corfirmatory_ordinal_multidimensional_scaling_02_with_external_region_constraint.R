setwd("//media//kswada//MyFiles//R//ocp")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  OCP
# ------------------------------------------------------------------------------

data("OCP", package = "smacof")

str(OCP)




# ------------------------------------------------------------------------------
# Regionally constrained MDS solution
# ------------------------------------------------------------------------------

Z <- OCP[, 56:57]

head(Z)



# ----------
# Fit a regionally constrained MDS solution bby forcing the items belonging to the same class
# to inhabit the same region in the MDS space.

# We use the final configuration of the first model as starting solution:  init = fit.ocp1$conf
# Through the constraint argument, we force the matrix C to be diagonal, and through constraint.type, we tell the funciton to perform
# a monotone transformation of the predictors in Z using the primary approach to ties (default)

fit.ocp2 <- smacofConstraint(ocpD, type = "ordinal", constraint = "diagonal", init = fit.ocp1$conf, external = Z, constraint.type = "ordinal")

fit.ocp2


op <- par(mar=c(5.1, 4.1, 4.1, 10.1), xpd = TRUE)
plot(fit.ocp2, col = cols, label.conf = list(col = cols), main = "OCP Configurations (Regional Restrictions)")
legend("right", legend = levels(OCP[,55]), inset=c(-0.48,0), col = colpal, pch = 19, title = "Facets")
par(op)
palette(pal)



# -->
# Stress-1 value of 0.318, which is considerable larger than the unrestricted stress value.
# The "openness to change" region is quite problematic in the sense that the region seems to be collapsed (degenerated).
# In addition, several points of the "conservation" class lie on the vertical border of the region.
# The unclassified items are nicely scattered in the space since we did not assign any restriction to them.

# We conclude that the regionally constrained MDS is too restrictive.
# Based on the exploratory MDS configuration, we could eliminate items that were far off from the remaining items in a class (e.g., i46, i10, i35, i36)
# and subsequently refit the restricted MDS without these itesm.



# ----------
# Shepard Diagram
par(mfrow = c(1,2))
plot(fit.ocp1, plot.type = "Shepard", main = "Shepard Diagram: Unrestricted")
plot(fit.ocp2, plot.type = "Shepard", main = "Shepard Diagram: Regional Constraint")


# -->
# We can see that solution with regional constraint is worse

