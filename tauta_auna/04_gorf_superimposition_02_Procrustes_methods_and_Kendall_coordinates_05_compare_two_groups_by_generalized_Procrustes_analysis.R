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
# Compare two groups by Generalized Procrustes analysis (GPA)
# ------------------------------------------------------------------------------

gor <- array(c(gorf.dat, gorm.dat), dim = c(8, 2, 59))


# Rotate configurations along their major axes, checking for ecentual reflection.
( gor_aligned <- aligne(gor) )



# ----------
# partial generalized Procrustes analysis
go <- pgpa(gor_aligned)


fe <- go$rotated[,,1:30]
ma <- go$rotated[,,31:59]

FE <- mshape(fe)
MA <- mshape(ma)



# ----------
par(mfrow=c(1,1))
plot(fe[,1,], fe[,2,], asp = 1, xlab = "", ylab = "", cex = 0.8, axes = F, pch = 20, col = "grey65")
points(ma[,1,], ma[,2,], pch = 4, cex = 0.8)


# 95% confidence ellipse
for(i in 1:8) lines(ELLI(ma[i,1,], ma[i,2,]))
for(i in 1:8) lines(ELLI(fe[i,1,], fe[i,2,]), lwd = 2, col = "grey65")

joinline <- c(1, 6:8, 2:5, 1)
lines(FE[joinline,], col = "grey65", lwd = 2)
lines(MA[joinline,])


