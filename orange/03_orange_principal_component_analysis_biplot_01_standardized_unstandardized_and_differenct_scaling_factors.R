setwd("//media//kswada//MyFiles//R//orange")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orange
# ------------------------------------------------------------------------------

orange <- read.table("orange.csv", header=TRUE, sep=";", dec=".", row.names=1)

str(orange)

dim(orange)


orange



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots
#   - singular value decomposition:  X = U * A ^ (1 - alpha) * A ^ alpha * V
#     A:  diagonal matrix with the singular values, lambda1^0.5, lambda2^0.5, ...
#     diagonal element of A ^ alpha is lambda1 ^ (alpha/2), lambda2 ^ (alpha/2), ...
#   - alpha is scaling factor
#       - alpha = 0:  row metric preserving, the plot approximates the Euclidean Distances among the persons in X
#       - alpha = 1:  column metric preserving, the plot approximates the covariance structure of the variables in X:
#                     the distances between the persons are determined by the Mahalanobis distance
#   - default choice in R's biplot is alpha = 1 (column metric preserving)
# ------------------------------------------------------------------------------


pca_or1 <- prcomp(orange[,1:7])

pca_or2 <- prcomp(orange[,1:7], scale = TRUE)


summary(pca_or1)

summary(pca_or2)




# -->
# for standardized version, PC1 67.77% and PC2 19.05% proportion of variances



# ------------------------------------------------------------------------------
# Principal Component Analysis:  loadings
# ------------------------------------------------------------------------------

data.frame(sensor = colnames(orange[,1:7]), round(pca_or2$rotation[,c("PC1", "PC2")], 3)) %>% arrange(desc(PC1))



# -->
# PC1 positive: Bitterness, Acidity, and Intensity of taste
# PC2 only Sweetness has positive value



data.frame(sensor = colnames(orange[,1:7]), round(pca_or2$rotation[,c("PC1", "PC2")], 3)) %>% arrange(desc(PC2))



# -->
# almost same solutions
# PC1 positive: Bitterness, Acidity, and Intensity of taste
# but the largest absolute value by Odour.typicality (= -0.452)
# smallest absolute value by Odour.intensity (= -0.211)


# PC2 only Sweetness has positive value  (almost same negative value for Bitterness)
# For PC2, Pulpiness and Odour.intensity contributes




# ------------------------------------------------------------------------------
# Correlation among variables
# ------------------------------------------------------------------------------

# the correlation of memory and visual, psychology is almost 0

round(cor(orange[,1:7]), 3)



# ------------------------------------------------------------------------------
# Standard deviation
# ------------------------------------------------------------------------------

# the sd's in a relative manner.
round(apply(orange[,1:7], 2, sd), 3)



# -->
# the sd of pupiness is relatively large




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots:  scale factor alpha = 1 (default)
#   - person:  PC scores
#   - response:  PC loadings
#   - column metric preserving
#     the plot approximates the covariance structure of the variables in X
#     the distance between the persons are determined by the Mahalanobis distance
#     Persons that are close to each other have similar response profiles in X
#   - the arrows of variables caovarying / correlating highly with each other should point into the same direction
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))



# pca_or1 Unstandardized:  approxiate the covariance structure
# the vector lengths reflect the sd's in a relatvie manner

biplot(pca_or1, pc.biplot = TRUE, cex = c(1, 1), col = c("black", "blue"),
       #       arrow.len = 0.05, asp = 1, 
       #       xlim = c(-4, 4),
       main = "Biplot (Unstandardized)", cex.axis = 0.8)




# ----------
# pca_or2 Standardized:  approxiate correlation structure
# the vector lengths sd information is lost, since all variables are scaled to sd = 1
# We could draw a unit circle; vectors close to this circle imply that these variables fit better

biplot(pca_or2, pc.biplot = TRUE, cex = c(1, 1), col = c("black", "blue"),
       #       arrow.len = 0.05, asp = 1,
       main = "Biplot (Standardized)", cex.axis = 0.8)



# -->
# now the vector length of Odour.intensity and Pulpiness is almost same
# (but sd of raw data is much different)

# but the vector length of Odour.intensity is very close to 1:  best fitted  (may by regression R2 is close to 1)

# Bitterness and Sweetness is almost reversed direction
# Odour.intensity and Pupiness contributes to PC2





# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots:  scale factor alpha = 0
#   - row metric preserving
#     Euclidean distance among persons
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))



# pca_or1 Unstandardized:  approxiate the covariance structure
# the vector lengths reflect the sd's in a relatvie manner

biplot(pca_or1, pc.biplot = TRUE, cex = c(1, 1), scale = 0, col = c("black", "blue"),
       #       arrow.len = 0.05, asp = 1, 
       #       xlim = c(-4, 4),
       main = "Biplot (Unstandardized)", cex.axis = 0.8)




# ----------
# pca_or2 Standardized:  approxiate correlation structure
# the vector lengths sd information is lost, since all variables are scaled to sd = 1
# We could draw a unit circle; vectors close to this circle imply that these variables fit better

biplot(pca_or2, pc.biplot = TRUE, cex = c(1, 1), scale = 0, col = c("black", "blue"),
       #       arrow.len = 0.05, asp = 1,
       main = "Biplot (Standardized)", cex.axis = 0.8)




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots:  comparison  alpha 0 / 1  *  unstandardized / standardized
# ------------------------------------------------------------------------------

# Biplot, with scale factor alpha = 1 (default):  row metric preserving
# Biplot, with scale factor alpha = 0:  column metric preserving

op <- par(mfrow = c(2,2), mar = c(4, 3, 4, 1))

biplot(pca_or1, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Unstandardized, ", alpha, "=0)")), scale = 0, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

biplot(pca_or1, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Unstandardized, ", alpha, "=1)")), scale = 1, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

biplot(pca_or2, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Standardized, ", alpha, "=0)")), scale = 0, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

biplot(pca_or2, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Standardized, ", alpha, "=1)")), scale = 1, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

par(op)


