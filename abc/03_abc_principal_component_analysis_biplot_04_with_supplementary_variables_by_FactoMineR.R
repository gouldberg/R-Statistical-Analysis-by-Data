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
# Principal Component Analysis Biplots by FactoMineR with supplementary variables:  quantitative variable
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------


# Supplementary variables with quantitative variable

graphics.off()
par(mfrow=c(1,2))


# "satis" and "recom" is for supplementary variable
res.pca1 <- FactoMineR::PCA(ABC2, quanti.sup = 1:2)



# -->
# quantitative variables (1 and 2) are added in variables factor map



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR with supplementary variables:  (quantitative variable +) qualitative variable
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


# Add "satis" and "recom" mapping into individual factor map
res.pca2 <- FactoMineR::PCA(ABC2, quali.sup = 1:2)

# plot(res.pca, invisible="quali")



# -->
# qualitative variables are added in individuals factor map



# -->
# How do you interpret this .....
