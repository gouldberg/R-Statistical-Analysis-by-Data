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
# Principal Component Analysis Biplots by FactoMineR
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca <- FactoMineR::PCA(orange, quanti.sup = 8:14, quali.sup = 15:16)




# ------------------------------------------------------------------------------
# Decomposition of variability per component
# ------------------------------------------------------------------------------

# PC1 and PC2 explains 86.8% of variance
res.pca$eig




# ------------------------------------------------------------------------------
# Correlation between variables and principal components
# ------------------------------------------------------------------------------
# Dim.1 is strongly positively correlated with the variables:  Odour.typicality and sweetness
# and strongly negatively correlated with the variables:  Bitterness and Acidity

# Dim.2 can be characterized by the variables:  Odour.intensity and Pupliness
res.pca$var$cor



# ----------
# Thus Tropicana fr. which has the highest coordinate on component 1, 
# has high values for odour typicality and sweetness and low values for the variables acidic and bitter.
orange["Tropicana fr.",]
res.pca$ind$coord




# ------------------------------------------------------------------------------
# Detecting outliers
# ------------------------------------------------------------------------------
# Distances from the individuals to the center of the cloud
round(res.pca$ind$dist,2)


# -->
# Tropicana fresh and Pampryl ambient is the two most extreme individuals



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


