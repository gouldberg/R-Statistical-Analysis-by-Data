setwd("//media//kswada//MyFiles//R//abc")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ABC
# ------------------------------------------------------------------------------

data("ABC", package = "MPsychoR")

str(ABC)

dim(ABC)

car::some(ABC)



# ----------
# convert factor to numeric
ABC2 <- rapply(ABC[,c(1,4,6:11)], f = as.numeric, classes = "factor", how = "replace")

str(ABC2)



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca <- FactoMineR::PCA(ABC2, quanti.sup = 1:2)




# ------------------------------------------------------------------------------
# Decomposition of variability per component
# ------------------------------------------------------------------------------

# PC1 and PC2 explains only 61.45% of variance
res.pca$eig




# ------------------------------------------------------------------------------
# Correlation between variables and principal components
# ------------------------------------------------------------------------------
res.pca$var$cor



# ----------
res.pca$ind$coord




# ------------------------------------------------------------------------------
# Detecting outliers
# ------------------------------------------------------------------------------
# Distances from the individuals to the center of the cloud
round(res.pca$ind$dist,2)




# ------------------------------------------------------------------------------
# Contribution of an individual or variable to the construction of a component
#   - Detecting those individuals that contribute to the construction of a principal component helps to evaluate the
#     component's stability.
#   - It is also interesting to evaluate the contribution of variables in constructing a component especially in nonstandardised PCA.
#   - To do so, we decompose the inertia of a component individual by individual (or variable by variable)
# ------------------------------------------------------------------------------

# Contributions of individuals:
round(res.pca$ind$contrib[,1:2],2)



# Contributions of variables
round(res.pca$var$contrib[,1:2],2)

