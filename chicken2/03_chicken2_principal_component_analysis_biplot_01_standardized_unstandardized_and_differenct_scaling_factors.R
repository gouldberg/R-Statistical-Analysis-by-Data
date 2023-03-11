setwd("//media//kswada//MyFiles//R//chicken2")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chicken2
# ------------------------------------------------------------------------------

chicken <- read.table("chicken.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(chicken)

dim(chicken)


car::some(chicken)



# ----------
chicken <- as.data.frame(t(chicken))

diet <- as.factor(c(rep("N", 6), rep("F16", 5), rep("F16R5", 8), rep("F16R16", 9), rep("F48", 6), rep("F48R24", 9)))

chicken <- cbind.data.frame(diet, chicken)

colnames(chicken)[1] <- "Diet"


dim(chicken)

str(chicken)



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots
#   - singular value decomposition:  X = U * A^(1-alph) * A^alpha * V
#     A:  diagonal matrix with the singular values, lambda1^0.5, lambda2^0.5, ...
#     diagonal element of A^alpha is lambda1^(alpha/2), lambda2^(alpha/2), ...
#   - alpha is scaling factor
#       - alpha = 1:  row metric preserving, the plot approximates the Euclidean Distances among the persons in X
#       - alpha = 0:  column metric preserving, the plot approximates the covariance structure of the variables in X:
#                     the distances between the persons are determined by the Mahalanobis distance
#   - default choice in R's biplot is alpha = 1 (column metric preserving)
# ------------------------------------------------------------------------------


pca_chic1 <- prcomp(chicken[,-1])

pca_chic2 <- prcomp(chicken[,-1], scale = TRUE)


summary(pca_chic1)

summary(pca_chic2)



# -->
# for standardized version, PC1 19.6% and PC2 9.4% proportion of variances




# ----------
# Biplot, with scale factor alpha = 1 (default):  row metric preserving
op <- par(mfrow = c(1,2))

biplot(pca_chic1, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = "Biplot (Unstandardized)", xlim = c(-4, 4), asp = 1, cex.axis = 0.8)

biplot(pca_chic2, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = "Biplot (Standardized)", asp = 1, cex.axis = 0.8)

layout(1)

par(op)



# ----------
# Biplot, with scale factor alpha = 1 (default):  row metric preserving
# Biplot, with scale factor alpha = 0:  column metric preserving
op <- par(mfrow = c(2,2), mar = c(4, 3, 4, 1))

biplot(pca_chic1, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Unstandardized, ", alpha, "=0)")), scale = 0, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

biplot(pca_chic1, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Unstandardized, ", alpha, "=1)")), scale = 1, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

biplot(pca_chic2, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Standardized, ", alpha, "=0)")), scale = 0, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

biplot(pca_chic2, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Standardized, ", alpha, "=1)")), scale = 1, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

par(op)



# -->
# Both left panels are now metric preserving (alpha = 0), meaning that they approximate the Euclidean distances among the cities.
# Both right panels are column metric preserving (alpha = 1), they approximate the covariance (top right) and correlation (bottom right) structure,
# in addition to the Mahalanobis distances between persons.


