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
# BrainIQ1 <- na.omit(BrainIQ[, -1])



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR with supplementary variables:  quantitative variable
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------


# Supplementary variables with quantitative variable (8 to 14)

graphics.off()
par(mfrow=c(1,2))


# Omit gender --> MRI_count as supplementary variables
# Note that FactoMineR impute automatically the missing values by the mean of the variables
res.pca1 <- FactoMineR::PCA(BrainIQ[,-1], quanti.sup = 6)



# -->
# quantitative variables (8 to 14) are added in variables factor map




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR with supplementary variables:  (quantitative variable +) qualitative variable
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

res.pca2 <- FactoMineR::PCA(BrainIQ, quali.sup = 1)

# plot(res.pca, invisible="quali")



# -->
# qualitative variables are added in individuals factor map



# -->
# How do you interpret this .....
