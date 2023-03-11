# setwd("//media//kswada//MyFiles//R//orange")
setwd("C:\\Users\\kouse\\Desktop\\R\\audiometric")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  audiometric
#   - see "A User's Guide to Principal Components" p.106
#   - the values are threshold measurements calibrated in units referred to as 'decibel loss' in comparison
#     to a reference standard for that particular instrument.
#     Observations are obtained, one ear at a time, for a number of frequencies
# ------------------------------------------------------------------------------

audiometric <- read.table("audiometric.txt", header=TRUE, sep="\t")

str(audiometric)

dim(audiometric)


audiometric



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by FactoMineR
#   - Variables factor map and individuals factor map
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))


res.pca0 <- FactoMineR::PCA(audiometric[,2:9])

res.pca0

summary(res.pca0)



# -->
# In variable factor map, a variable is always represented within a circle of radius 1
# Dim1 and Dim2 are orthogonal and a variable cannot be strongly related to 2 orthogonal components simultaneously.
# The variables are scaled by default


# ----------
# Decomposition of variability per component
# PC1 and PC2 explains 69.34% of variance
res.pca0$eig


# ----------
# Correlation between variables and principal components
res.pca0$var$cor


# ----------
# inidividual coordinate
res.pca0$ind$coord



# ----------
# Distances from the individuals to the center of the cloud
round(res.pca0$ind$dist,2)


# ----------
# Contribution of an individual or variable to the construction of a component
#   - Detecting those individuals that contribute to the construction of a principal component helps
#     to evaluate the component's stability.
#   - It is also interesting to evaluate the contribution of variables in constructing a component especially in nonstandardised PCA.
#   - To do so, we decompose the inertia of a component individual by individual (or variable by variable)

# Contributions of individuals:
round(res.pca0$ind$contrib[,1:2],2)



# ----------
# Automatic dimension description from the quantitative and categorical variables
#   - For a quantitative variable, the correlation coefficient between the coordinates of the individuals on the component and each variable
#     is calculated.
#     We then sort the variables in descending order from the highest coefficient to the weakest and retain the variables with the highest correlation coefficients
#   - For a categorical variable, we conduct a one-way analysis of variance where we seek to explain the coordinates of the individuals (on the component)
#     by the categorical variable.
#     We use the sum to zero contrasts (sum of all to 0)
#     For each categorical variable, a Student t-test is conducted to compare the average of the individuals who possess that category with the general average.

dimdesc(res.pca0)

# ----------
# lapply(dimdesc(res.pca0),lapply,round,2)







