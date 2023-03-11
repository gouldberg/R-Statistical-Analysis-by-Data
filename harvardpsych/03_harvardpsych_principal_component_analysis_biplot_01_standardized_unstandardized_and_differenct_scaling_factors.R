setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HarvardPsych
# ------------------------------------------------------------------------------

data("HarvardPsych", package = "MPsychoR")

str(HarvardPsych)

dim(HarvardPsych)


# researchers in rows, words in columns  (29 * 43)
head(HarvardPsych)



# ----------
rownames(HarvardPsych)

colnames(HarvardPsych)




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


pca_or1 <- prcomp(HarvardPsych)

pca_or2 <- prcomp(HarvardPsych, scale = TRUE)


summary(pca_or1)

summary(pca_or2)



# -->
# for standardized version, PC1 16.9% and PC2 9.9% proportion of variances




# ------------------------------------------------------------------------------
# Principal Component Analysis:  loadings
# ------------------------------------------------------------------------------

data.frame(theme = colnames(HarvardPsych), round(pca_or2$rotation[,c("PC1", "PC2")], 3)) %>% arrange(desc(PC1))



# -->
# PC1 positive: visual, representation, information ...
# PC1 negative: american, societry, association ...



data.frame(theme = colnames(HarvardPsych), round(pca_or2$rotation[,c("PC1", "PC2")], 3)) %>% arrange(desc(PC2))



# -->
# PC2 positive:  behavior, brain, selfcontrol, behavioral ...
# PC2 negative:  visual, representation, information ...




# ----------
data.frame(researcher = rownames(HarvardPsych), round(pca_or2$x[,c("PC1", "PC2")], 3)) %>%
        filter(researcher %in% c("Schacter", "Buckner", "Buckholtz", "Xu", "Caramazza", "Nakayama", "Banaji"))



# -->
# Note that Schacter and Buckner are typical in PC1 but NOT in PC2 ...




# ------------------------------------------------------------------------------
# Correlation among variables
# ------------------------------------------------------------------------------

# the correlation of memory and visual, psychology is almost 0

round(cor(HarvardPsych[,c("memory", "visual", "psychology", "social")]), 3)



# ------------------------------------------------------------------------------
# Standard deviation
# ------------------------------------------------------------------------------

# the sd's in a relative manner.
round(apply(HarvardPsych[,c("memory", "visual", "psychology", "social")], 2, sd), 3)





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


