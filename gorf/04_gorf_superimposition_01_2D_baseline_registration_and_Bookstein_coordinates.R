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
# Baseline registration for 2D data
#   - shapes::bookstein2d() performs a baseline registration for 2D data
# ------------------------------------------------------------------------------

( Bf <- shapes::bookstein2d(gorf.dat) )

( Bm <- shapes::bookstein2d(gorm.dat) )

names(Bf)



# ----------
# plot the resulting superimposition
graphics.off()

par(mfrow = c(1,2))
plotshapes(Bf$bshpv)
plotshapes(Bm$bshpv)



# ------------------------------------------------------------------------------
# Baseline registration for 2D data:  more customized graphs
# ------------------------------------------------------------------------------

# select two landmarks
land_1 <- 1
land_2 <- 2

Fe <- booksteinA(gorf.dat, land_1, land_2)
Ma <- booksteinA(gorm.dat, land_1, land_2)



graphics.off()
layout(matrix(c(1,2), 1, 2))

# Note that two landmarks (1 and 2) along x-axis are aligned
plot(mbshape(gorf.dat, land_1, land_2), pch = 18, asp = 1, xlab = "", ylab = "", axes = F, col = "red")
points(mbshape(gorm.dat, land_1, land_2), pch = 22, col = "blue")

lines(mbshape(gorm.dat, land_1, land_2)[c(1, 6, 7, 8, 2, 3, 4, 5, 1),], col = "blue")
lines(mbshape(gorf.dat, land_1, land_2)[c(1, 6, 7, 8, 2, 3, 4, 5, 1),], lty = 2, col = "red")

plot(Fe[,1,], Fe[,2,], asp = 1, axes = F, xlab = "", ylab = "", cex = 0.5, pch = 18, col = "red")
points(Ma[,1,], Ma[,2,], cex = 0.5, pch = 22, col = "blue")
segments(-0.5, 0, 0.5, 0, lw = 2)



# -->
# The baseline corresponds to the location of two landmarks for the 2D registration



# ------------------------------------------------------------------------------
# Check singular values of th evariance-covariance matrix: the last four singular values should be null
# ------------------------------------------------------------------------------

# the set of all possible shapes = 2 * p(# of landmarks) = 16
# 1 landmarks are set at coordinate and 2nd landmark used for registration
# registration needs 1 translation, 1 scaling and 1 rotation for removing location, orientation and scale effect

# --> 16 - 4 = 12 singular values

dim(gorf.dat)

Fe <- booksteinA(gorf.dat, land_1, land_2)

round(svd(var(matrix(Fe, 30, 16, byrow = T)))$d, 6)






