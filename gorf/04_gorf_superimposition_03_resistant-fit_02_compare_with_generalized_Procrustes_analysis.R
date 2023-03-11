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
# Compare Generalized Resistant-fit superimposition and Generalized Procrustes superimposition
#
#  - Resistant-fit superimposition:  robust regression using repeated medians
#  - Full Procrustes superimposition:  least-squares method
# ------------------------------------------------------------------------------


# Generalized Procrustes Analysis
gls <- pgpa(gorm.dat)

glsr <- gls$rotated



# ----------
# Generalized Resistant-fit Analysis
grf <- grf2(gorm.dat)

grfr <- grf$rotated



# ----------
graphics.off()

par(mfrow=c(1,2), mar = c(1,1,1,1))

plot(glsr[,1,], glsr[,2,], pch = 20, col = "grey65", asp = 1, axes = F, xlab = "", ylab = "", main = "Generalized Procrustes Analysis")
points(gls$mshape, pch = 3, cex = 2, lwd = 2)


plot(grfr[,1,], grfr[,2,], pch = 20, col = "grey65", asp = 1, axes = F, xlab = "", ylab = "", main = "Generalized Resistant-fit Analysis")
points(grf$medshape, pch = 3, cex = 2, lwd = 2)



# -->
# The generalized resistant-fit procedure can be more efficient than Procrustes superimposition for finding landmarks
# that bear more variation than others.