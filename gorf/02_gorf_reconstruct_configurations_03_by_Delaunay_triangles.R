setwd("//media//kswada//MyFiles//R//golf")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf
# ------------------------------------------------------------------------------

shapes::gorf.dat

str(gorf.dat)


( M <- gorf.dat[,,3] )



# ----------
# modify the data
# M[6,2] <- -M[6,2]



# ------------------------------------------------------------------------------
# Reconstruct configurations with Delaunay triangles
#   - select a minimal number of distances for covering information on the object
# ------------------------------------------------------------------------------

# see previous analysis
Ma

truss



# ----------
( dd <- geometry::delaunayn(Ma) )


par(mfrow = c(1,1))
plot(Ma, asp = 1, axes = F, xlab = "", ylab = "")
trimesh(dd, Ma, add = T)



# -->
# Note however that unlike the truss network method, reconstructing the relative position of the landmarks can be ambiguous.
