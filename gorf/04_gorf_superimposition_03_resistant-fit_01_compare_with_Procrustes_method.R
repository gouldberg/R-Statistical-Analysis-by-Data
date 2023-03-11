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
# Compare Resistant-fit superimposition and ordinary full Procrustes superimposition
#
#  - Resistant-fit superimposition:  robust regression using repeated medians
#  - Full Procrustes superimposition:  least-squares method
# ------------------------------------------------------------------------------

M1 <- gorf.dat[,,1]

M2 <- gorf.dat[,,2]

M1[1,2] <- 250


lsq <- fPsup(M1, M2)

resf <- oPsup(M1, M2)



# ----------
joinline <- c(1, 6:8, 2:5, 1)

par(mfrow=c(1,2), mar = c(1,1,1,1))

plot(lsq[[1]], asp = 1, axes = F, xlab = "", ylab = "", main = "Ordinary Full Procrustes superimposition")
lines(lsq[[1]][joinline,])
points(lsq[[2]], pch = 3)
lines(lsq[[2]][joinline,], lty = 3)

plot(resf[[1]], asp = 1, axes = F, xlab = "", ylab = "", main = "Orthogonal resistant-fit superimposition")
lines(resf[[1]][joinline,])
points(resf[[2]], pch = 3)
lines(resf[[2]][joinline,], lty = 3)



# -->
# Resistant-Fit is better


