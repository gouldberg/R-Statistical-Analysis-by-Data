setwd("//media//kswada//MyFiles//R//yaass")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  yaass
# ------------------------------------------------------------------------------

data("yaass", package = "MPsychoR")

str(yaass)



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR with supplementary variables:  quantitative variable
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------


# Supplementary variables with quantitative variable (4 and 5)

graphics.off()
par(mfrow=c(1,2))


res.pca1 <- FactoMineR::PCA(yaass[,1:5], quanti.sup = 4:5)



# -->
# quantitative variables (4 and 5) are added in variables factor map




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR with supplementary variables:  (quantitative variable +) qualitative variable
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

res.pca2 <- FactoMineR::PCA(yaass[,1:6], quali.sup = 6)

# plot(res.pca, invisible="quali")



# -->
# qualitative variables are added in individuals factor map



# -->
# How do you interpret this .....
