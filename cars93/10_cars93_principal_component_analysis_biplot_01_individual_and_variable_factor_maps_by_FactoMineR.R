rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\01_異常検知\\cars93")



# ------------------------------------------------------------------------------
# data:  Cars93
# ------------------------------------------------------------------------------


library(MASS)


str(Cars93)


car::some(Cars93)





# ------------------------------------------------------------------------------
# Select variables and transform data
# ------------------------------------------------------------------------------


# select 15 variables + "Type"

cc <- c("Type", "Min.Price", "Price", "Max.Price", "MPG.city", "MPG.highway", "EngineSize", "Horsepower", "RPM", "Rev.per.mile",
        "Fuel.tank.capacity", "Length", "Wheelbase", "Width", "Turn.circle", "Weight")


Xc <- Cars93[,cc]


rownames(Xc) <- Cars93[,"Make"]


head(Xc)



# ----------
# column is Make and row is variable

summary(Xc)




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca <- FactoMineR::PCA(Xc, quali.sup = 1)

res.pca



summary(res.pca)



# -->
# In variable factor map, a variable is always represented within a circle of radius 1
# Dim1 and Dim2 are orthogonal and a variable cannot be strongly related to 2 orthogonal components simultaneously.

# The variables are scaled by default




# ------------------------------------------------------------------------------
# Automatic dimension description
# ------------------------------------------------------------------------------

library(FactoMineR)


dimdesc(res.pca)

dimdesc(res.pca)$Dim.1

dimdesc(res.pca)$Dim.2




# ----------
# change threshold
dimdesc(res.pca, proba = 0.2)




# ------------------------------------------------------------------------------
# Choosing the number of dimensions to examine
# ------------------------------------------------------------------------------

res.pca$eig



# ----------
par(mfrow=c(1,1))

barplot(res.pca$eig[,1], main = "Eigenvalues", names.arg = paste("dim", 1:nrow(res.pca$eig)))



# -->
# PC1 and PC2 explains 81.7% of variance
# In other words, 81.7% of the total variabiligy of the cloud of individuals (or variables) is represented by the plane.




# ------------------------------------------------------------------------------
# Studying the Cloud of Individuals
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))

plot(res.pca, invisible = "var")



# -->
# The cloud of individuals representation is a default output of the PCA function.
# The supplementary categorical variables are also represented on the graph of individuals through their categories.


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


# Here, coloured according to the 1st variable "Type"
plot(res.pca, choix = "ind", habillage = 1, cex = 0.7)




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

colnames(tmp) <- colnames(Xc)[2:15]

tmp



# ----------
# standardized data
round(scale(Xc[,2:15]), digits = 3)



# -->
# standardized data is useful to facilitate comparison of the data with the average in terms of number of standard deviations,
# but also to compare one variable's values to another.



# ----------
# correlation matrix
round(cor(Xc[,2:15]), digits = 2)



