setwd("C:\\Users\\kswad\\Desktop\\R\\orange")

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
# Principal Component Analysis Biplots by FactoMineR with supplementary variables:  quantitative variable
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------


# Supplementary variables with quantitative variable (8 to 14)

graphics.off()
par(mfrow=c(1,2))

res.pca1 <- FactoMineR::PCA(orange[,1:14], quanti.sup = 8:14)



# -->
# quantitative variables (8 to 14) are added in variables factor map




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR with supplementary variables:  quantitative variable + qualitative variable
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

res.pca2 <- FactoMineR::PCA(orange, quanti.sup = 8:14, quali.sup = 15:16)

# plot(res.pca, invisible="quali")



# -->
# qualitative variables (15 and 16) are added in individuals factor map



# -->
# Variables factor map: 
# The analysis of this sensory perception is reinforced by the variables pH and saccharose.
# Indeed, these two variables are positively correlated with the first component and lie on the side of the orange juices
# perceived as sweet and slightly acidic (a high pH index indicates low acidity).

# One also finds the reaction known as "saccharose inversion" (or hydrolysis):
# The saccharose breaks down into glucose and fructose in an acidic environment.
# THe acidic orange juices thus contain more fructose and glucose, and less saccharose than the average.


# -->
# Individuals factor map: 
# Category variables remain at the barycenter of the individuals in their plane representation.
# Categorical variable can thus be regarded as the mean individual obtained from the set of individuals who have it.
# It seems that sensory perception of the products differs according to their packaging (way of preserving)
# (despite the fact that they were all tasted at the same temperature)

