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
# Principal Component Analysis Biplots by FactoMineR
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

res.pca <- FactoMineR::PCA(chicken, quali.sup = 1)



# ------------------------------------------------------------------------------
# Automatic dimension description
# ------------------------------------------------------------------------------

dimdesc(res.pca, proba = 1e-5)



# -->
# For the 1st component, the genes that are the most correlated to that component are all positively correalted.
# Thus, these genes are underexpressed when the chickens have been fasting for 48 hours.
# For component2, some genes are overexpressed when the chickens are refed after fasting for 48 hours while other genes are underexpressed.

# The chickes that suffered 48 hours of stress have significantly lower coordinates than the others on component 1,
# whether refed afterwards or not.
# Inversely, chickens that have suffered 16 hours of stress and been refed afterwards have significantly positive coordinates.
# Component 2 is characterised by chickens subjected to 48 hours of stress and opposes chickens refed afterwards (with significantly positive coordinates)
# with those that were not (with significantly negative coordinates).



