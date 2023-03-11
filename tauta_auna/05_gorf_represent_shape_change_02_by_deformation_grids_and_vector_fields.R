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
# Deformation grids
#   - Deformation grids are formalizations of D'arcy Thompson's idea
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
# Deformation grid between the mean configurations for the gorilla skulls belonging to females and males
par(mfrow=c(1,1))

tps(FE, MA, 20)

joinline <- c(1, 6:8, 2:5, 1)
lines(FE[joinline,], col = "grey50", lwd = 2)
lines(MA[joinline,], lty = 3, lwd = 2)



# -->
# Note that deformation grids and Thin-Plate Splines can map a form onto another,
# and they are independent of the scale factor.

# The left (maxillary region) part is enlarged while the neurocranial part is smaller in the males in comparison to the females.



# ------------------------------------------------------------------------------
# Display of field of vectors
# ------------------------------------------------------------------------------

library(sp)

joinline <- c(1, 6:8, 2:5, 1)

sFE <- spsample(Polygon(FE[joinline,]), 200, type = "regular")

sR <- sFE@coords

sT <- tps2d(sR, FE, MA)

for(i in 1:dim(sR)[1]){
  arrows(sR[i,1], sR[i,2], sT[i,1], sT[i,2], length = 0.05)
}


