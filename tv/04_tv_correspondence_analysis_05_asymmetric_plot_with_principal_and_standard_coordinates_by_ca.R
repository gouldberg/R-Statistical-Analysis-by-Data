setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")

TV

dim(TV)

str(TV)


TV2 <- margin.table(TV, c(1,3))

TV2



# ------------------------------------------------------------------------------
# Correspondence Analysisis Biplot by ca:  Asymmetric CA plot
#   - One set of connections between correspondence analysis and the biplot can be seen through the reconstitution formula,
#     giving the decomposition of the correspondence matrix in terms of the standard coordinates Phi and Gamma.
#   - Asymmetric CA plot of row principal coordinates and the column standard coordinates is a biplot that approximates the deviations
#     of the contingency ratios from their values under independence.
#
#   - Principal coordinates:
#      The coordinates of the row and column profiles with respect to their own principal axes are defined so that the inertia along each axis
#      is the corresponding eigenvalue value
#   - Standard coordinates:
#      A rescaling of the principla coordinates to unit inertia along each axis.
#
#   - Symmetic map:  The joint plot in principal coordinates is called the symmetric map because both row and column profiles are overlaid in the same coordinate system
#   - Asymmetric map:  shows one set of points in principal coordinates and the other set in standard coordinates
# ------------------------------------------------------------------------------

library(ca)


fit_ca <- ca(TV2)



# ----------
# It is typical in such biplots to display one set of coordinates as ponts and the other as vectors from the origin,
# as controlled by the arrows argument,
# so that one can interpret the data values represented as approximated by the projections of the points on the vectors


graphics.off()
par(mfrow = c(2,2))
plot(fit_ca, arrows = c(FALSE, TRUE))


# row principal coordinates and column standard coordinates
plot(fit_ca, arrows = c(FALSE, TRUE), map = "rowprincipal", main = "rowprincipal")


# column principal coordinates and row standard coordinates
plot(fit_ca, arrows = c(TRUE, FALSE), map = "colprincipal", main = "colprincipal")


