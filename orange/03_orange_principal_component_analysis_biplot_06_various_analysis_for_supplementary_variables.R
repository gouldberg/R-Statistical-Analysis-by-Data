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
# Qualitative supplementary variable
# ------------------------------------------------------------------------------


res.pca$quali.sup


res.pca$quali.sup$coord




# ------------------------------------------------------------------------------
# Quantitative supplementary variable
# ------------------------------------------------------------------------------


res.pca$quanti.sup$coord


