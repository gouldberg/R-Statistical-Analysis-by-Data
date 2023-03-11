setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)


dim(tension)



# -----------
tens <- t(as.matrix(tension[, 1:800]))



# ------------------------------------------------------------------------------
# standardize individual series
# ------------------------------------------------------------------------------


std <- diag(1 / sqrt(diag(cov(tens))))


tens <- as.matrix(tens) %*% std




# ------------------------------------------------------------------------------
# Principal component analysis
# ------------------------------------------------------------------------------


m1 <- princomp(tens)


names(m1)


summary(m1)




# ----------
par(mfrow = c(1,1))

screeplot(m1)



# -->
# we may select PCs = 3
# the first 3 components exlain about 60.0% of the total variability



# ------------------------------------------------------------------------------
# Compute loading matrix
# ------------------------------------------------------------------------------

# square root of eigenvalues

sdev <- m1$sdev



# number of PCs

pc_n <- 3


SD <- diag(sdev[1:pc_n])


M <- m1$loadings


L <- M[,1:pc_n] %*% SD


print(round(L, 3))



# -->
# the loading is not clear ...




# ------------------------------------------------------------------------------
# Covariance matrix of errors
# ------------------------------------------------------------------------------

LLt <- L %*% t(L)


diag(LLt)


sigE <- 1 - diag(LLt)


summary(sigE)



# -->
# the variances of the noise components are between 12.9 to 81.5% of each standardized series
# indicating that marked variablity remains in each series.




# ------------------------------------------------------------------------------
# Estimated commmon factors
# ------------------------------------------------------------------------------

library(MTS)


head(m1$scores)


MTSplot(m1$scores[,1:3])





