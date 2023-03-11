# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_unempstatesAdj")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-unempstatesAdj
# ------------------------------------------------------------------------------

da <- read.csv("m-unempstatesAdj.txt", sep = "", header = T)


str(da)


dim(da)


car::some(da)



# ----------
# first difference

drate <- diffM(da)




# ------------------------------------------------------------------------------
# standardize individual series
# ------------------------------------------------------------------------------


std <- diag(1 / sqrt(diag(cov(drate))))


drates <- as.matrix(drate) %*% std




# ------------------------------------------------------------------------------
# Principal component analysis
# ------------------------------------------------------------------------------


m1 <- princomp(drates)


names(m1)


summary(m1)




# ----------
par(mfrow = c(1,1))

screeplot(m1)



# -->
# we may select only PCs = 1...
# the first component exlain about 50.5% of the total variability



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





# ------------------------------------------------------------------------------
# Covariance matrix of errors
# ------------------------------------------------------------------------------

LLt <- L %*% t(L)


diag(LLt)


sigE <- 1 - diag(LLt)


summary(sigE)



# -->
# the variances of the noise components are between 20 to 79% of each standardized drate series,
# indicating that marked variablity remains in each drate series



# ------------------------------------------------------------------------------
# Estimated commmon factors
# ------------------------------------------------------------------------------


head(m1$scores)


par(mfrow = c(1,1), mar = c(2,2,2,2))

MTSplot(m1$scores[,1:3])

