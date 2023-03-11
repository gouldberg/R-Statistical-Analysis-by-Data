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
# Principal Component Analysis Biplots by FactoMineR with supplementary variables
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

res.pca <- FactoMineR::PCA(chicken, quali.sup = 1)



# ----------
summary(res.pca, nb.dec = 3)



# ------------------------------------------------------------------------------
# Choosing the number of dimensions to examine
# ------------------------------------------------------------------------------

res.pca$eig



# ----------
par(mfrow=c(1,1))
barplot(res.pca$eig[,1], main = "Eigenvalues", names.arg = paste("dim", 1:nrow(res.pca$eig)))



# -->
# PC1 and PC2 explains 29.0% of variance
# In other words, 29.0% of the total variabiligy of the cloud of individuals (or variables) is represented by the plane.
# Note that here we obtain 42 dimensions at most, which corresponds to the number of individuals -1 (and not the total number of variables):
# the 43 individuals are therefore in a space with 42 dimensions at most.



# ------------------------------------------------------------------------------
# Studying the Cloud of Individuals
# ------------------------------------------------------------------------------

# colouring the individuals according to the variable Diet

par(mfrow=c(1,1))
plot(res.pca, habillage = 1, invisible = "var", cex = 0.7)



# ----------
# Coordinate
res.pca$ind$coord



# ----------
# Contribution of individuals for 1st to 4th dimension
round(sort(res.pca$ind$contrib[,1], decreasing = TRUE), digits = 3)
round(sort(res.pca$ind$contrib[,2], decreasing = TRUE), digits = 3)
round(sort(res.pca$ind$contrib[,3], decreasing = TRUE), digits = 3)
round(sort(res.pca$ind$contrib[,4], decreasing = TRUE), digits = 3)



# -->
# The principal plane of the PCA separates the chickens into two subgroups.
# Those that have undergone intense stress (48 hours of fasting), are greatly disperse, and those that have been subject to less intense stress (16 hours of fasting),
# and more concentrated and located close to the origin.
# Furthermore, the first component separates the chickens into 3 groups: chickens that have undergone intense stress but that have not been refed afterwards (F48),
# chickens that have undergone intense stress and that have been refed afterwards (F48R24),
# and the other chickens.
# Chickens which have been refed tend to recover from intense stress and their health tends to be similar to that of normal chickens.
# However, 24 hours of feeding is not enough for the state of the chicken to completely return to normal.
# This means that some genes are specific to a state of intense stress, as some genes are overexpressed under stress while others are underestimated
# (the graph of variables shows that certain variables are negatively correlated while others are positively correlated).

# The second component is specific to the chickens F48R24.



# ----------
# 3rd and 4th dimension
plot(res.pca, habillage = 1, invisible = "var", cex = 0.7, axes = 3:4)



# -->
# Chickens that followed a normal diets have negative coordinates on component 3
# and chickens who suffered 16 days of stress have positive coordinates on the same component.
# Chickens that were refed after 16 days of stress are between thest two groups, with a gradient depending on duration of feeding time:
# chickens refed for 5 hours are closer to those that were not refed, and chickes refed for 16 hours are close to those that did not suffer stress.
# It therefore seems that some genes are expressed differently according to whether there was a stress of 16 hours or not,
# when some genes return gradually to a "normal" state. However, even after 16 hours of feeding, the genes do not function normally again.



# ------------------------------------------------------------------------------
# Studying the supplementary categorical variable
# ------------------------------------------------------------------------------

res.pca$quali.sup



# ----------
# Comfidence ellipses can be drawn around the categories of a supplementary categorical variable
# (i.e., around the barycentre of the individuals characterised by the category)
# These ellipses are well adpated to plane representation and enable us to visualise whther or not two categories differ significantly

plotellipses(res.pca, cex = 0.8)



# -->
# The visual impression is  that the chickens under excessive stress (F48 and F48R24)
# are very different from the others.
# However, one can also see that the confidence ellipses are disjointed for diets F16 and F16R16, F16R16 and N, or F16 and F16R5:
# this differentiation of the diests was not at all obvious without the confidence ellipses.


# ----------
plotellipses(res.pca, cex = 0.8, axes = 3:4)



# -->
# On plane 3-4, several categories of the variable "Diet" are clearly distinguishable: diet N is different from all the others and in particular from F16R16.
# This indicates that the chickens which underwent 16 days of stress and which were refed for the following 16 days still did not recover from thier stress.



# ------------------------------------------------------------------------------
# Studying the Cloud of Variables
# ------------------------------------------------------------------------------

# To examine whether or not there is a particular structure to the variables, we provide one point per variable (with no arrows or labels)
par(mfrow=c(1,1))
plot(res.pca, choix = "var", invisible = "var")
points(res.pca$var$coord[,1:2], pch = ".")



# -->
# The cloud is regularly distributed and does not need to be commented (but nonetheless had to be verified).



# ----------
# Coordinate
res.pca$var$coord



# ----------
# Contribution of variables for 1st and 2nd dimension
round(sort(res.pca$var$contrib[,1], decreasing = TRUE), digits = 3)

round(sort(res.pca$var$contrib[,2], decreasing = TRUE), digits = 3)

res.pca$quanti.sup$coord



# ----------
# 3rd and 4th dimension
plot(res.pca, choix = "var", invisible = "var", axes = 3:4)
points(res.pca$var$coord[,3:4], pch = ".")



