setwd("//media//kswada//MyFiles//R//decathlon")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  decathlon
# ------------------------------------------------------------------------------

data("decathlon", package = "FactoMineR")

str(decathlon)

dim(decathlon)


decathlon



# ------------------------------------------------------------------------------
# Correspondence Analysisis Biplot by ca:  different scaling
#   - for better visual properties in terms of representing the relations between the rows and column categories,
#     particularly when the strength of association (inertia) in the data is low
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(2,2))


# rows shown in principal coordinates and the columns in standard coordinates multiplied by the columns mass of the corresponding point
plot(fit_ca, arrows = c(TRUE, FALSE), map = "rowgab", main = "rowgab")


# columns shown in principal coordinates and the rows in standard coordinates multiplied by the rows mass of the corresponding point
plot(fit_ca, arrows = c(FALSE, TRUE), map = "colgab", main = "colgab")


# Show more directly the contributions of the vectors to the low-dimensional solution
# provide a reconstruction of the standardized residuals from independence, using the points in standard coordinates multiplied by the square root of the corresponding masses
plot(fit_ca, arrows = c(TRUE, FALSE), map = "rowgreen", main = "rowgreen")


plot(fit_ca, arrows = c(FALSE, TRUE), map = "colgreen", main = "colgreen")



# ----------
par(mfrow = c(1,1))
plot(fit_ca, map = "rowgreen", main = "rowgreen")

plot(fit_ca, map = "colgreen", main = "colgreen")
