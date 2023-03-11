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
# Simple Correspondence Analysis by FactoMineR  with supplementary variables
# ------------------------------------------------------------------------------

# In correspondence analysis, we provide the matrix as supplementary variables (as "col.sup" not "qunati.sup")

par(mfrow = c(1,1))
res.ca2 <- CA(orange[,1:14], col.sup = 8:14)

summary(res.ca2)





# ----------
# compare to PCA biplot
res.pca2 <- FactoMineR::PCA(orange, quanti.sup = 8:14, quali.sup = 15:16)

