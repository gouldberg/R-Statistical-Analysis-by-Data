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
# Principal Component Analysis Biplots by FactoMineR
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

res.pca <- FactoMineR::PCA(temperature, ind.sup = 24:35, quanti.sup = 13:16, quali.sup = 17)




# ------------------------------------------------------------------------------
# Automatic dimension description
# ------------------------------------------------------------------------------

dimdesc(res.pca)


dimdesc(res.pca)$Dim.1

dimdesc(res.pca)$Dim.2



# ----------
# change threshold
dimdesc(res.pca, proba = 0.2)

