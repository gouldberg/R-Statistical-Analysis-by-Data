setwd("//media//kswada//MyFiles//R//iris")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf
# ------------------------------------------------------------------------------

gorf.dat

str(gorf.dat)



# ------------------------------------------------------------------------------
# Mean shape
# ------------------------------------------------------------------------------

# gorf.dat and gorm.dat
# select two landmarks

land_1 <- 1
land_2 <- 2


graphics.off()
layout(matrix(c(1,2), 1, 2))

Fe <- booksteinA(gorf.dat, land_1, land_2)
Ma <- booksteinA(gorm.dat, land_1, land_2)


plot(mbshape(gorf.dat, land_1, land_2), pch = 18, asp = 1, xlab = "", ylab = "", axes = F)
points(mbshape(gorm.dat, land_1, land_2), pch = 22)

lines(mbshape(gorm.dat, land_1, land_2)[c(1, 6, 7, 8, 2, 3, 4, 5, 1),])
lines(mbshape(gorf.dat, land_1, land_2)[c(1, 6, 7, 8, 2, 3, 4, 5, 1),], lty = 2)

plot(Fe[,1,], Fe[,2,], asp = 1, axes = F, xlab = "", ylab = "", cex = 0.5, pch = 18)
points(Ma[,1,], Ma[,2,], cex = 0.5, pch = 22)
segments(-0.5, 0, 0.5, 0, lw = 2)



# -->
# The baseline corresponds to the location of two landmarks for the 2D registration



# ------------------------------------------------------------------------------
# Check singular values of th evariance-covariance matrix: the last four singular values should be null
# ------------------------------------------------------------------------------

Fe <- booksteinA(gorf.dat, land_1, land_2)

round(svd(var(matrix(Fe, 30, 16, byrow = T)))$d, 6)



a <- mbshape(gorf.dat, land_1, land_2)





