setwd("//media//kswada//MyFiles//R//gorf")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf and gorm
# ------------------------------------------------------------------------------

dim(gorf.dat)

dim(gorm.dat)



# ------------------------------------------------------------------------------
# Delaunay triangle and calculate the angles at vertices for each triangle
# ------------------------------------------------------------------------------

gorm <- gorm.dat[,,4]



# ----------
# Delaunay triange
tri <- geometry::delaunayn(gorm)

tri


graphics.off()
par(mfrow=c(1,1), mar = c(1,1,1,1))

plot(gorm, asp = 1, axes = F)
n <- dim(tri)[1]
for(i in 1:n){ polygon(gorm[tri[i,],]) }
# alternatively
# trimesh(tri, gorm, add = T)



# ----------
# calculate the angles at vertices for each triangle
anglerao(gorm, tri)



# ------------------------------------------------------------------------------
# Calculate mean shape matrix from a configuration set and a triangulation matrix
# ------------------------------------------------------------------------------

# Note that we can apply tri for all configurations
msh <- meanrao(gorm.dat, tri)



# mean shape angular matrix
msh$mean



# matrix of 1 and 0 indicating, respectively, the largest angle and the two smallest angles for each triangle 
msh$maxangle



# ------------------------------------------------------------------------------
# Compute the shape variance-covariance from a set of configurations
# ------------------------------------------------------------------------------

vcvrao(gorm.dat, tri)



# ------------------------------------------------------------------------------
# REconstruct configuration from the angle matrix and triangulation
# ------------------------------------------------------------------------------

ra <- raoinv(msh$mean, tri)


graphics.off()
par(mfrow=c(1,2), mar = c(1,1,1,1))

plot(gorm, asp = 1, axes = F, main = "original one config")
n <- dim(tri)[1]
for(i in 1:n){ polygon(gorm[tri[i,],]) }
# alternatively
# trimesh(tri, gorm, add = T)

plot(ra, asp = 1, axes = F, main = "Mean shape reconstructed from mean shape angular matrix")
text(ra + 0.05, labels = 1:8, cex = 0.7, col = "red")

