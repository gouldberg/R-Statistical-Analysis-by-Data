setwd("//media//kswada//MyFiles//R//gorf")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf, panf, pongof
# ------------------------------------------------------------------------------
data(gorf.dat, package = "shapes")
data(panf.dat, package = "shapes")
data(pongof.dat, package = "shapes")

dim(gorf.dat)

dim(panf.dat)

dim(pongof.dat)



# ------------------------------------------------------------------------------
# Check configurations by sample
# ------------------------------------------------------------------------------

idx_gf <- sample(dim(gorf.dat)[3], size = 2, replace = FALSE)
idx_pf <- sample(dim(panf.dat)[3], size = 2, replace = FALSE)
idx_pgf <- sample(dim(pongof.dat)[3], size = 2, replace = FALSE)


par(mfrow=c(2,3), mar=c(2,2,2,2))
plot(gorf.dat[,,idx_gf[1]], axes = FALSE, xlab = "", ylab = "", main = paste("gorf", idx_gf[1]))
polygon(gorf.dat[,,idx_gf[1]])

plot(gorf.dat[,,idx_gf[2]], axes = FALSE, xlab = "", ylab = "", main = paste("gorf", idx_gf[2]))
polygon(gorf.dat[,,idx_gf[2]])

plot(panf.dat[,,idx_pf[1]], axes = FALSE, xlab = "", ylab = "", main = paste("panf", idx_pf[1]))
polygon(panf.dat[,,idx_pf[1]])

plot(panf.dat[,,idx_pf[2]], axes = FALSE, xlab = "", ylab = "", main = paste("panf", idx_pf[2]))
polygon(panf.dat[,,idx_pf[2]])

plot(pongof.dat[,,idx_pgf[1]], axes = FALSE, xlab = "", ylab = "", main = paste("pongf", idx_pgf[1]))
polygon(pongof.dat[,,idx_pgf[1]])

plot(pongof.dat[,,idx_pgf[2]], axes = FALSE, xlab = "", ylab = "", main = paste("pongf", idx_pgf[2]))
polygon(pongof.dat[,,idx_pgf[2]])



# ----------
# dataset must be first reorganized because landmarks are not labeled in the same order for each set.

gorf <- gorf.dat
panf <- panf.dat[c(5,1,2:4,6:8),,]
pongof <- pongof.dat[c(5,1,2:4,6:8),,]

# gorf <- gorf.dat[c(5,1,2:4,6:8),,]
# panf <- panf.dat
# pongof <- pongof.dat

APE <- array(c(panf, gorf, pongof), dim = c(8,2,80))



# ------------------------------------------------------------------------------
# Use Linear Discriminant Analysis and reconstruct configurations
# corresponding to extreme values along discriminant axes to determine differences between groups.
# ------------------------------------------------------------------------------

# !!! refer to previous analysis !!!


# Linear discriminant analysis
mod2 <- lda(m, fact)


# -->
# Note the warning message that appears due to the fact that matrices of variance-covariance are not singular
# We could have avoided this by substituting original data by their projections on the p - 4 PC-axes: the result would have been strictly similar.

mod2



# ----------
# Estimate the within group variance-covariance
VCVw <- SSer / dfer


# LD axes are premultiplied by the within variance-covariance matirx
LDs <- VCVw %*% mod2$scaling



# ----------
# Calculate averaged shapes for each group and extremes
( msh <- apply(m, 2, mean) )

score <- predict(mod2)$x

LD1M <- (matrix(msh + max(score[,1]) * LDs[,1], 8, 2))
LD1m <- (matrix(msh + min(score[,1]) * LDs[,1], 8, 2))
LD2M <- (matrix(msh + max(score[,2]) * LDs[,2], 8, 2))
LD2m <- (matrix(msh + min(score[,2]) * LDs[,2], 8, 2))



# ----------
joinline <- c(1, 6:8, 2:5, 1, NA, 7, 4)

graphics.off()

layout(matrix(c(1,1,2,3), 2, 2))

plot(score, pch = as.character(fact), asp = 1)

par(mar = c(5,1,2,1))

plot(LD1M, axes = F, frame = F, asp = 1, xlab = "", ylab = "", pch = 22, main = "LD1")
lines(LD1M[joinline,], lty = 1)
points(LD1m, pch = 17)
lines(LD1m[joinline,], lty = 2)

plot(LD2M, axes = F, frame = F, asp = 1, xlab = "", ylab = "", pch = 22, main = "LD2")
lines(LD2M[joinline,], lty = 1)
points(LD2m, pch = 17)
lines(LD2m[joinline,], lty = 2)



# -->
# On the projections of individuals, we notice that no individual has been misclassified.
# The 1st linear discriminant axis mainly shows difference between the Asian and the African species,
# while the second axis opposes the gorilla to the chimpanzee and orangutan.
# The orangutan has a more triangular skull cross section, while the chimpanzee and gorilla are more rectangular.

# On the second axis, the chimpanzee is opposed to both gorilla and orangutan: differences between species mainly concern the facial part of the skull.
# the base of the skull in chimpanzee is upturned anteriorly, while it is inflected downwards for both other species.


