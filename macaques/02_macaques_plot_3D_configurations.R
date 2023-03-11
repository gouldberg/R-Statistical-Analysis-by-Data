setwd("//media//kswada//MyFiles//R//macaques")


packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  macaques
# ------------------------------------------------------------------------------

data(macaques, package = "shapes")
data(macf.dat, package = "shapes")
data(macm.dat, package = "shapes")

str(macaques)

dim(macaques$x)

macaques



# ----------
dim(macf.dat)

dim(macm.dat)



# ------------------------------------------------------------------------------
# plot 3D coordinates
# ------------------------------------------------------------------------------

joins <- c(1,2,5,2,3,4,1,6,5,3,7,6,4,7)

colpts <- rep(1:7, times = 18)



# ----------
# shapes3d uses the rgl library in R, which in turn uses OpenGL graphics.
# The 3D plots can be easily rotated and moved in the graphics window. 
# Each landmarks displayed by a different colour.
shapes3d(macaques$x[,,1], col = colpts, joinline = joins)


shapes3d(macaques$x, col = colpts, joinline = joins)
