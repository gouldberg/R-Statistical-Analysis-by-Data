setwd("//media//kswada//MyFiles//R//gorf")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf and gorm
# ------------------------------------------------------------------------------

dim(gorf.dat)

dim(gorm.dat)



# merge female and male gorilla's data
gor <- array(c(gorf.dat, gorm.dat), dim = c(8, 2, 59))



# ------------------------------------------------------------------------------
# Check configurations by sample
# ------------------------------------------------------------------------------

idx_f <- sample(dim(gorf.dat)[3], size = 2, replace = FALSE)
idx_m <- sample(dim(gorm.dat)[3], size = 2, replace = FALSE)


par(mfrow=c(2,2), mar=c(2,2,2,2))
plot(gorf.dat[,,idx_f[1]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("female-", idx_f[1]))
polygon(gorf.dat[,,idx_f[1]])

plot(gorf.dat[,,idx_f[2]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("female-", idx_f[2]))
polygon(gorf.dat[,,idx_f[2]])

plot(gorm.dat[,,idx_m[1]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("male-", idx_m[1]))
polygon(gorm.dat[,,idx_m[1]])

plot(gorm.dat[,,idx_m[2]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("male-", idx_m[2]))
polygon(gorm.dat[,,idx_m[2]])



# ------------------------------------------------------------------------------
# Consists of superimposing configurations
# ------------------------------------------------------------------------------

# aligne():  align the configurations along their first principal axes
GOR <- aligne(gor)


idx <- sample(dim(GOR)[3], size = 2, replace = FALSE)


par(mfrow=c(2,2), mar=c(2,2,2,2))
plot(gor[,,idx[1]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("original-", idx[1]))
polygon(gor[,,idx[1]])

plot(gor[,,idx[2]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("original-", idx[2]))
polygon(gor[,,idx[2]])

plot(GOR[,,idx[1]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("aligned-", idx[1]))
polygon(GOR[,,idx[1]])

plot(GOR[,,idx[2]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("aligned-", idx[2]))
polygon(GOR[,,idx[2]])



# ----------
# partial generalized Procrustes analysis
go <- pgpa(GOR)


idx <- sample(dim(go$rotated)[3], size = 2, replace = FALSE)

par(mfrow=c(2,2), mar=c(2,2,2,2))
plot(GOR[,,idx[1]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("aligned-", idx[1]))
polygon(GOR[,,idx[1]])

plot(GOR[,,idx[2]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("aligned-", idx[2]))
polygon(GOR[,,idx[2]])

plot(go$rotated[,,idx[1]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("PGPA-", idx[1]))
polygon(go$rotated[,,idx[1]])

plot(go$rotated[,,idx[2]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("PGPA-", idx[2]))
polygon(go$rotated[,,idx[2]])




# ------------------------------------------------------------------------------
# Compute tangent space coordinates and PCA
# ------------------------------------------------------------------------------

# Compute tangent space coordinates
gos <- orp(go$rotated)


idx <- sample(dim(go$rotated)[3], size = 2, replace = FALSE)

par(mfrow=c(2,2), mar=c(2,2,2,2))
plot(go$rotated[,,idx[1]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("PGPA-", idx[1]))
polygon(go$rotated[,,idx[1]])

plot(go$rotated[,,idx[2]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("PGPA-", idx[2]))
polygon(go$rotated[,,idx[2]])

plot(gos[,,idx[1]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("Tangent Space-", idx[1]))
polygon(gos[,,idx[1]])

plot(gos[,,idx[2]], axes = FALSE, asp = 1, xlab = "", ylab = "", main = paste("Tangent Space-", idx[2]))
polygon(gos[,,idx[2]])




# ----------
m <- t(matrix(gos, 16, 59))

pcs <- prcomp(m)

summary(pcs)


par(mfrow=c(1,1))
biplot(pcs)



# ------------------------------------------------------------------------------
# Plot PCA scores, variance explained by each PCA
# ------------------------------------------------------------------------------

par(mfrow=c(1,2), mar = c(4,4,1,1))

plot(pcs$x, pch = c(rep("f", 30), rep("m", 29)))

barplot(pcs$sdev ^ 2 / sum(pcs$sdev ^ 2), ylab = "% of variance")

title(sub = "PC Rank", mgp = c(0,0,0))



# -->
# Males and females occupy different positions on the 1st two PCs of shape variation.



# ------------------------------------------------------------------------------
# Plot mean and extreme configurations
# ------------------------------------------------------------------------------

mesh <- as.vector(mshape(gos))


max1 <- matrix(mesh + max(pcs$x[,1]) * pcs$rotation[,1], 8, 2)
min1 <- matrix(mesh + min(pcs$x[,1]) * pcs$rotation[,1], 8, 2)
max2 <- matrix(mesh + max(pcs$x[,2]) * pcs$rotation[,2], 8, 2)
min2 <- matrix(mesh + min(pcs$x[,2]) * pcs$rotation[,2], 8, 2)


joinline <- c(1, 6:8, 2:5, 1, NA, 7, 4)

plot(min1, axes = F, frame = F, asp = 1, xlab = "", ylab = "", pch = 22)
points(mshape(gos), pch = 15, col = gray(0.6))
points(max1, pch = 17)
title(sub = "PC1", mgp = c(-4, 0, 0))
lines(max1[joinline,], lty = 1)
lines(mshape(gos)[joinline,], lty = 1, col = gray(0.6))
lines(min1[joinline,], lty = 2)

plot(min2, axes = F, frame = F, asp = 1, xlab = "", ylab = "", pch = 22)
points(mshape(gos), pch = 15, col = gray(0.6))
points(max2, pch = 17)
title(sub = "PC2", mgp = c(-4, 0, 0))
lines(max2[joinline,], lty = 1)
lines(mshape(gos)[joinline,], lty = 1, col = gray(0.6))
lines(min2[joinline,], lty = 2)


# -->
# Segment appearing inside the skull outline approximately corresponds to the boundary between the face and the neurocranium

# 1st PC opposes skull outlines that are rather elongated antero-posteriorly (males) and more globular skulls (females)
# 2nd PC opposes forms having relatively smaller neurocranium and more pronounced faces (males)



# ----------
# alternatively
par(mfrow=c(1,1), mar=c(2,2,2,2))
shapes::shapepca(shapes::procGPA(gor))



# ------------------------------------------------------------------------------
# Visualize shape variation along PC by using thin-plate splines
# ------------------------------------------------------------------------------

msh <- mshape(gos)

tps(msh, min1, 12)
points(min1, pch = 21, bg = "black")
title("PC1: left extreme")

tps(msh, max1, 12)
points(max1, pch = 22, bg = "black")
title("PC1: right extreme")



# ------------------------------------------------------------------------------
# Visualize uniform deformation (affine deformation)
#   - Affine changes and variation deal with global flatterning, sharing, or dilation that may possibly receive a simple interpretation
# ------------------------------------------------------------------------------

gou <- uniform2D(gor)



idx <- sample(dim(gou$rotated)[3], size = 2, replace = FALSE)

par(mfrow=c(2,2), mar=c(2,2,2,2))
plot(gos[,,idx[1]], axes = FALSE, xlab = "", ylab = "", main = paste("Projected-", idx[1]))
polygon(gos[,,idx[1]])

plot(gos[,,idx[2]], axes = FALSE, xlab = "", ylab = "", main = paste("Projected-", idx[2]))
polygon(gos[,,idx[2]])

plot(gou$rotated[,,idx[1]], axes = FALSE, xlab = "", ylab = "", main = paste("Superimposed and Projcted-", idx[1]))
polygon(gou$rotated[,,idx[1]])

plot(gou$rotated[,,idx[2]], axes = FALSE, xlab = "", ylab = "", main = paste("Superimposed and Projected-", idx[2]))
polygon(gou$rotated[,,idx[2]])



# ----------
# mean shape
msh <- gou$meanshape


# uniform component variation
Un <- gou$uniform


layout(matrix(c(1,1,1,1,2,4,3,5), 2, 4))

par(mar=c(5,4,4,2))
plot(gou$scores, pch = c(rep("f", 30), rep("m", 29)), asp = 1)


par(mar=c(4,1,4,1))
tps(msh, matrix(as.vector(msh) + Un[,1] * max(gou$scores[,1]), 8, 2), 12)
title("U1: left")

tps(msh, matrix(as.vector(msh) + Un[,2] * max(gou$scores[,2]), 8, 2), 12)
title("U2: top")

tps(msh, matrix(as.vector(msh) + Un[,1] * min(gou$scores[,1]), 8, 2), 12)
title("U1: right")

tps(msh, matrix(as.vector(msh) + Un[,2] * min(gou$scores[,1]), 8, 2), 12)
title("U2: bottom")



# ------------------------------------------------------------------------------
# Visualize NON affine deformation (non uniform components of shape variation)
#  - This component corresponds to a general bending of the configuration
# ------------------------------------------------------------------------------

kp <- dim(gor)[2] * dim(gor)[1]

n <- dim(gor)[3]


# matrix of n by kp aligned coordinates
X <- t(matrix(gou$rotated, kp, n))


# V:  the matrix of Procrustes residuals
# msh: mean shape
V <- X - t(t(rep(1, n))) %*% as.vector(msh)


# project matrix in the component space based on Burnaby method
Ben <- diag(1, kp) - Un %*% solve(t(Un) %*% Un) %*% t(Un)


# project Procrustes residuals in the component space
LSR <- svd(V %*% Ben)

score <- LSR$u %*% diag(LSR$d)

NonUnif <- LSR$v



# ----------
graphics.off()

layout(matrix(c(1,1,1,1,2,3,4,5), 2, 4))

plot(score[,1:2], pch = c(rep("f", 30), rep("m", 29)), xlab = "RW1", ylab = "RW2")

tps(msh, matrix(as.vector(msh) + NonUnif[,1] * max(score[,1]), 8, 2), 20)
title("RW 1: right")

tps(msh, matrix(as.vector(msh) + NonUnif[,1] * max(score[,1]), 8, 2), 20)
title("RW 1: left")

tps(msh, matrix(as.vector(msh) + NonUnif[,2] * min(score[,2]), 8, 2), 20)
title("RW 2: top")

tps(msh, matrix(as.vector(msh) + NonUnif[,2] * min(score[,2]), 8, 2), 20)
title("RW 2: bottom")


# -->
# The second component represents a differential of dilatation between the anterior and posteriro parts of the skull cross section
# Differences between sexes are not characterized by this kind of morphological deformation



# ----------
paste("Nonaffine variation = ", round(sum(diag(var(score[,1:(kp - 6)]))), 5))

paste("Affine variation = ", round(sum(diag(var(score[,1:2]))), 5))

paste("Total variation = ", round(sum(diag(var(X))), 5))



# -->
# Total shape variation is indeed equal to the sum of the nonaffine and affine variation


# ----------
diag(var(gou$score[,1:2])) / sum(diag(var(X)))

round(diag(var(score[,1:(kp - 6)])) / sum(diag(var(X))), 4)



# -->
# The variation explained by affine transformation summarizes 26% of the total variation.
# The second uniform component (dilatation) accounts for 14%
# The nonaffine components summarize variation that decrease with their rank, the 1st explaining nearly 34% of the total shape variation 
