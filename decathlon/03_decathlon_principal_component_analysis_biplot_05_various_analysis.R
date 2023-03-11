setwd("//media//kswada//MyFiles//R//decathlon")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  decathlon
# ------------------------------------------------------------------------------

data("decathlon", package = "FactoMineR")

str(decathlon)

dim(decathlon)


decathlon



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

res.pca <- FactoMineR::PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)




# ------------------------------------------------------------------------------
# Choosing the number of dimensions to examine
# ------------------------------------------------------------------------------

res.pca$eig



# ----------
par(mfrow=c(1,1))
barplot(res.pca$eig[,1], main = "Eigenvalues", names.arg = paste("dim", 1:nrow(res.pca$eig)))



# -->
# PC1 and PC2 explains 50.1% of variance
# In other words, 50% of the total variabiligy of the cloud of individuals (or variables) is represented by the plane.

# It may be interesting to compare this percentage with the 0.95-quantile of the distribution of the percentages obtained by simulating
# data tables of equivalent size on the basis of a normal distribution.
# This quantile obtained for 40 individuals and 10 variables is worth 38%: even if the percentage 50% seems relatively low,
# it expresses a significant structure in the data.



# ------------------------------------------------------------------------------
# Studying the Cloud of Individuals
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(res.pca, invisible = "var")



# -->
# The cloud of individuals representation is a default output of the PCA function.
# The supplementary categorical variables are also represented on the graph of individuals through their categories.

# Bourguignon and Karpov have very different performance profiles since they are opposed according to the main axis of variability.
# Casarsa seems to be an unusual athlete in that his results are extreme for both the first and second principla components.



# ----------
# Coordinate
res.pca$ind$coord



# ----------
# Contribution of individuals for 1st and 2nd dimension
round(sort(res.pca$ind$contrib[,1], decreasing = TRUE), digits = 3)

round(sort(res.pca$ind$contrib[,2], decreasing = TRUE), digits = 3)




# ----------
# 3rd and 4th dimension
plot(res.pca, choix = "ind", axes = 3:4)



# ----------
# Individuals can be colour-coded according to categorical variables

# Here, coloured according to the 13th variable "Competition"
plot(res.pca, choix = "ind", habillage = 13, cex = 0.7)



# ------------------------------------------------------------------------------
# Studying the supplementary categorical variable
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(res.pca, invisible = c("ind", "var"))


res.pca$quali.sup



# ----------
# Comfidence ellipses can be drawn around the categories of a supplementary categorical variable
# (i.e., around the barycentre of the individuals characterised by the category)
# These sllipses are well adpated to plane representation and enable us to visualise whther or not two categories differ significantly

plotellipses(res.pca, cex = 0.8)



# ------------------------------------------------------------------------------
# Studying the Cloud of Variabeles
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(res.pca, choix = "var")



# -->
# The variables 100m and long jump are negatively correlated.
# therefore, an athlete who runs 100 metres quickly will generally jump a long way.
# The variables 100m, 400m, and 110 hurdles are positively correlated, that is, some athletes perform well in all four events while others do not.
# Overall, the variables relating to speed are negatively correlated with the 1st principal component while the variables shot put and 
# long jump are positively correlated with this component.



# ----------
# Coordinate
res.pca$var$coord



# ----------
# Contribution of variables for 1st and 2nd dimension
round(sort(res.pca$var$contrib[,1], decreasing = TRUE), digits = 3)

round(sort(res.pca$var$contrib[,2], decreasing = TRUE), digits = 3)




# ----------
# 3rd and 4th dimension
plot(res.pca, choix = "var", axes = 3:4)




# ------------------------------------------------------------------------------
# Joint analysis of the Cloud of Individuals and the Cloud of Variables
# ------------------------------------------------------------------------------

# means and standard deviations by variable
means_ <- round(res.pca$call$centre, digits = 3)
sd_ <- round(res.pca$call$ecart.type, digits = 3)
tmp <- rbind(means_, sd_)

colnames(tmp) <- colnames(decathlon)[1:10]

tmp



# ----------
# standardized data
round(scale(decathlon[,1:12]), digits = 3)


# -->
# standardized data is useful to facilitate comparison of the data with the average in terms of number of standard deviations,
# but also to compare one variable's values to another.



# ----------
# correlation matrix
round(cor(decathlon[,1:12]), digits = 2)



# ---------
psych::pairs.panels(decathlon[,c(1,2,6,10)])




