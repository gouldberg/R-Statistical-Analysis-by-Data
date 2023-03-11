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
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca0 <- FactoMineR::PCA(temperature[,1:12])

res.pca0

summary(res.pca0)



# -->
# In variable factor map, a variable is always represented within a circle of radius 1
# Dim1 and Dim2 are orthogonal and a variable cannot be strongly related to 2 orthogonal components simultaneously.

# The variables are scaled by default


