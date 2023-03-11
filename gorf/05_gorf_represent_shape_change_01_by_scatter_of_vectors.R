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
# Display differences between a couple of shapes with a field of arrows
# ------------------------------------------------------------------------------

gor <- array(c(gorf.dat, gorm.dat), dim = c(8, 2, 59))


# partial generalized Procrustes analysis
go <- pgpa(aligne(gor))

fe <- go$rotated[,,1:30]

ma <- go$rotated[,,31:59]



# ----------
# mean shape
FE <- mshape(fe)

MA <- mshape(ma)



# ----------
joinline <- c(1, 6:8, 2:5, 1)


graphics.off()
  
par(mfrow=c(1,2), mar = c(1,1,1,1))

plot(FE, asp = 1, xlab = "", ylab = "", axes = F)
lines(FE[joinline,], col = "grey50", lwd = 2)
lines(MA[joinline,], lty = 3)
arrows(FE[,1], FE[,2], MA[,1], MA[,2], length = 0.1, lwd = 2)
title("No amplification")


mag <- 3
MA1 <- MA + (MA - FE) * 3

plot(FE, asp = 1, xlab = "", ylab = "", axes = F)
lines(FE[joinline,], col = "grey50", lwd = 2)
lines(MA[joinline,], lty = 3)
arrows(FE[,1], FE[,2], MA1[,1], MA1[,2], length = 0.1, lwd = 2)
title("3-times amplification")



