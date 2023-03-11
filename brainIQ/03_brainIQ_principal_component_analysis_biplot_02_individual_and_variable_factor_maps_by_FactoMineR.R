setwd("//media//kswada//MyFiles//R//brainIQ")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brainIQ
# ------------------------------------------------------------------------------

data("BrainIQ", package = "MPsychoR")

str(BrainIQ)



# ----------
# omit NAs and gender
BrainIQ1 <- na.omit(BrainIQ[, -1])



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca0 <- FactoMineR::PCA(BrainIQ1)

res.pca0

summary(res.pca0)



# -->
# In variable factor map, a variable is always represented within a circle of radius 1
# Dim1 and Dim2 are orthogonal and a variable cannot be strongly related to 2 orthogonal components simultaneously.

# The variables are scaled by default


