setwd("//media//kswada//MyFiles//R//temperature")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  temperature
# ------------------------------------------------------------------------------

temperature <- read.table("temperature.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(temperature)

dim(temperature)


temperature



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR with supplementary variables
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------


# Each country will be represented by the climate of its capital.
# The data of the other cities are not taken into account to avoid giving more weight to the countries for which several cities are listed.
# Thus, the capitals will be regarded as active individuals while the other cities will be regarded as supplementary individuals

graphics.off()
par(mfrow=c(1,2))

res.pca <- FactoMineR::PCA(temperature, ind.sup = 24:35, quanti.sup = 13:16, quali.sup = 17)



# ----------
summary(res.pca, nb.dec = 2)



# ------------------------------------------------------------------------------
# Choosing the number of dimensions to examine
# ------------------------------------------------------------------------------

res.pca$eig



# ----------
par(mfrow=c(1,1))
barplot(res.pca$eig[,1], main = "Eigenvalues", names.arg = paste("dim", 1:nrow(res.pca$eig)))



# -->
# PC1 and PC2 explains 98.3% of variance
# In other words, 98.3% of the total variabiligy of the cloud of individuals (or variables) is represented by the plane.
# and from two synthetic variables, we are abel to summarise most of the information provided by the 12 initial variables.

# It may be interesting to compare this percentage with the 0.95-quantile of the distribution of the percentages obtained by simulating
# data tables of equivalent size on the basis of a normal distribution.
# This quantile obtained for 23 individuals and 12 variables is worth 39%.
# Here  expresses a significant structure in the data.



# ------------------------------------------------------------------------------
# Studying the Cloud of Individuals
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(res.pca, invisible = "var")



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
# Athens contribute at largest to 1st dimension
# Reykjavik contribute at largest to 2nd dimension
# Stockholm contribute at largest to 3rd dimension  (although explained variance for 3rd dimension is small)
# Madrid contribute at largest to 4th dimension  (although explained variance for 4th dimension is small)



# ----------
# 3rd and 4th dimension
plot(res.pca, choix = "ind", axes = 3:4)



# ----------
# Individuals can be colour-coded according to categorical variables

# Here, coloured according to the 17th variable "Area"
plot(res.pca, choix = "ind", habillage = 17, cex = 0.7)



# ------------------------------------------------------------------------------
# Studying the supplementary categorical variable
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(res.pca, invisible = c("ind", "var"))


res.pca$quali.sup



# ----------
# Comfidence ellipses can be drawn around the categories of a supplementary categorical variable
# (i.e., around the barycentre of the individuals characterised by the category)
# These ellipses are well adpated to plane representation and enable us to visualise whther or not two categories differ significantly

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

res.pca$quanti.sup$coord


# -->
# Interestingly, October is largest contributor to 1st dimension and smallest contributor to 2nd dimension
# Similarly, June is smallest contributor to 1st dimension and largest contributor to 1st dimension
# October is almost aligned with "Annual"

# This first component can be summarised by the term "average annual temperature".
# This summary is reinforced by a correlation coefficient of 0.998 beetween this principal component and the illustrative variable average annual temperature

# September, October, and April are more closely linked than the others to this first component.
# They represent the best the annual temperature.
# Apart from the average annual temperature, another supplementary quantitative variable is linked to the 1st principal component: latitude.
# The correlation between latitude and the first principal component is worth -0.85, which means that the cities that are further sourth (lower latitude)
# have a higher coordinate on the 1st component and are therefore the warmest cities.

# November and March are strongly corrleated: indeed, the ends of the arrows are close to the circle of correlation,
# so the angle between vectors November and March in the space (space of the individuals, cities) is close to the angle on the plane, namely, close to zero.
# As the correlation coefficient is the cosine of the angle in the individuals' space, the correlation coefficient is close to 1.
# This means that the cities wehre it is cold in November are also those where it is cold in March.


# Second dimension can summed up by the opposition "summer" -- "bad season".
# It is important to note that this opposition has nothing to do with an evolution of the averages,
# since the data are centered prior to analysis.
# This opposition reflects the fact that, for a given annual temperature, some cities are relatively rather hot in summer and others rather cold.
# "Annual thermal amplitude" is related to this 2nd component, which can be connected to the two following facts:
# - the highest values of this variable were observed for most continental cities
# - the lowest values were observed for thte cities nearest to the Atlantic
# "Longitude" is linked to this component, but the relationship is not particularly strong (correlation = 0.4196)
# 2nd dimension clearly distinguishes coastal cities with low thermal amplitude from continental cities with high thermal amplitude.



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

colnames(tmp) <- colnames(temperature)[1:12]

tmp



# ----------
# standardized data
round(scale(temperature[,1:12]), digits = 3)



# -->
# standardized data is useful to facilitate comparison of the data with the average in terms of number of standard deviations,
# but also to compare one variable's values to another.



# ----------
# correlation matrix
round(cor(temperature[,1:12]), digits = 2)



# ---------
psych::pairs.panels(temperature[,1:12])




