setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HarvardPsych
# ------------------------------------------------------------------------------

data("HarvardPsych", package = "MPsychoR")

str(HarvardPsych)

dim(HarvardPsych)


# researchers in rows, words in columns  (29 * 43)
head(HarvardPsych)



# ----------
rownames(HarvardPsych)

colnames(HarvardPsych)




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca0 <- FactoMineR::PCA(HarvardPsych)

res.pca0

summary(res.pca0)



# -->
# In variable factor map, a variable is always represented within a circle of radius 1
# Dim1 and Dim2 are orthogonal and a variable cannot be strongly related to 2 orthogonal components simultaneously.

# The variables are scaled by default


