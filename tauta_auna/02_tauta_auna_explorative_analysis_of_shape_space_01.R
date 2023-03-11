setwd("//media//kswada//MyFiles//R//tauta_auna")


packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  tauta and auna
# ------------------------------------------------------------------------------

tauta <- read.table("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//tauta.R")

auna <- read.table("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//auna.R")

str(tauta)

str(auna)



# ----------
# The datasets are stored in tow data frames, of k columns, and n * p rows.
# For convenience, we transform them in 2 array objects, tauta and auna, of respective dimensions

taut <- array(NA, dim = c(64, 2, 29))

aun <- array(NA, dim = c(64, 2, 31))

taut[,1,] <- tauta[,1];  taut[,2,] <- tauta[,2]

aun[,1,] <- auna[,1];  aun[,2,] <- auna[,2]



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# there is no landmark available on the teeth;
# therefore the elongation of the 1st ellipse is used for aligning specimens and standardizing measurements.
# the same is true for the starting point.

# Perform standardized elliptic Fourier analysis
coe <- matrix(NA, 29, 32 * 4)

for(i in 1:29){
  
  N <- NEF(taut[,,i])
  
  coe[i,] <- c(N$A, N$B, N$C, N$D)
}


coe1 <- matrix(NA, 31, 32 * 4)

for(i in 1:31){
  
  N <- NEF(aun[,,i])
  
  coe1[i,] <- c(N$A, N$B, N$C, N$D)
}



coef <- rbind(coe, coe1)

dim(coef)


co <- coef ^ 2

( tharm <- apply(co, 2, sum) )

( power <- apply(matrix(tharm, 32, 4), 1, sum) )


round(cumsum(power[-1]) / sum(power[-1]), 3)[1:9]



# -->
# The first 7 harmonics totalize more than 99% of the total power remaining after first removing variation caontained
# in the relative length of the first ellipse.



# ------------------------------------------------------------------------------
# Examine cumulative variance explained by coefficients
# ------------------------------------------------------------------------------

( vharm <- apply(coef, 2, var) )

( variation <- apply(matrix(vharm, 32, 4), 1, sum) )


round(cumsum(variation[-1]) / sum(variation[-1]), 3)[1:18]



# -->
# The results are sensibly similar.
# Even in excluding the 1st harmonic, the first 7 following ones totalize more than 92% of the total reconstructable variation.
# This means that by taking only 7 harmonics, we are not losing too much information.
# It also means that most of the tooth variation is described by a few harmonic coefficients.



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# PCA applied to the first 8 - 1 harmonics
pc <- princomp(coef[,c(2:8, 32+2:8, 64+2:8, 96+2:8)])

(pc$sdev ^ 2 / sum(pc$sdev ^ 2))[1:5]

mshcoef <- apply(coef[,c(1:8, 32+1:8, 64+1:8, 96+1:8)], 2, mean)

ev <- pc$loadings

Mx1 <- mshcoef + max(pc$score[,1]) * c(ev[1:7,1], 0, ev[8:14,1], 0, ev[15:21,1], 0, ev[22:28,1], 0)

mx1 <- mshcoef + min(pc$score[,1]) * c(ev[1:7,1], 0, ev[8:14,1], 0, ev[15:21,1], 0, ev[22:28,1], 0)

Mx2 <- mshcoef + max(pc$score[,2]) * c(ev[1:7,2], 0, ev[8:14,2], 0, ev[15:21,2], 0, ev[22:28,2], 0)

mx2 <- mshcoef + min(pc$score[,2]) * c(ev[1:7,2], 0, ev[8:14,2], 0, ev[15:21,2], 0, ev[22:28,2], 0)


Mx1 <- iefourier(Mx1[1:8], Mx1[9:16], Mx1[17:24], Mx1[25:32], 8, 64)

mx1 <- iefourier(mx1[1:8], mx1[9:16], mx1[17:24], mx1[25:32], 8, 64)

Mx2 <- iefourier(Mx2[1:8], Mx2[9:16], Mx2[17:24], Mx2[25:32], 8, 64)

mx2 <- iefourier(mx2[1:8], mx2[9:16], mx2[17:24], mx2[25:32], 8, 64)




# ----------
layout(matrix(c(1,1,1,1,2,3), 2, 3))

plot(pc$score, pch = c(rep("a", 29), rep("t", 31)), asp = 1)

plot(Mx1, type = "l", col = "grey60", asp = 1, frame = F, axes = F, main = "PC1", xlab = "", ylab = "", lwd = 2)
points(mx1, type = "l", asp = 1)

plot(Mx2, type = "l", col = "grey60", asp = 1, frame = F, axes = F, main = "PC2", xlab = "", ylab = "", lwd = 2)
points(mx2, type = "l", asp = 1)



# -->
# Black and thin outlines correspond to minimal values along axes,
# and gray and thick outlines to maximal scores

# The first two principal components oppose both species relatively well.
# The species "tauta" seems to have a more complex and angular outline than the species "auna"



# The higher order harmonic coefficients should contain very little information concerning differentiation between groups,
# and are more likely to contain noise because of digitization error.
# When using the standardization, one can drop the first 3 coefficients of the 1st harmonic they are constant.
# The last one is concerned with the relative elongation of the 1st ellipse.

# Prior to digitization, teeth were numerized under a stereographic camera.
# Depending on their orientation below the camera, teeth could actually look wider or longer.
# We choose therefor to forget the d1 coefficient for limiting measurement error caused by orientation of teeth below the objective of the stereographic camera.
# 
