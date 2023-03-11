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
# Represent shape changes of TPS (Thin-Plate Splines) by the way of contour and image plot
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
library(sp)

joinline <- c(1, 6:8, 2:5, 1)

sFE <- spsample(Polygon(FE[joinline,]), 100, type = "regular")
sFE1 <- spsample(Polygon(FE[joinline,]), 10000, type = "regular")

sR <- sFE@coords
sR1 <- sFE1@coords

sT <- tps2d(sR, FE, MA)
sT1 <- tps2d(sR1, FE, MA)

def <- sqrt(apply((sT1 - SR1) ^ 2, 1, sum))

x1 <- length(unique(sR1[,1]))

y1 <- length(unique(sR1[,2]))

im <- matrix(NA, x1, y1)

xind <- (1:x1)[as.factor(rank(sR1[,1]))]

yind <- (1:y1)[as.factor(rank(sR1[,2]))]

n <- length(xind)

for(i in 1:n){ im[xind[i], yind[i]] <- def[i] }



# ----------
graphics.off()

par(mfrow=c(1,2), mar = c(1,1,1,1))

plot(FE, asp = 1, xlab = "", ylab = "", axes = F)
lines(FE[joinline,])
contour(sort(unique(sR1[,1])), sort(unique(sR1[,2])), im, axes = F, frame = F, add = T)

image(sort(unique(sR1[,1])), sort(unique(sR1[,2])), im, col = gray((32:0)/32)[1:26], asp = T, axes = F, frame = F)
for(i in 1:dim(sR)[1]){ arrows(sR[i,1], sR[i,2], sT[i,1], sT[i,2], length = 0.04)}

