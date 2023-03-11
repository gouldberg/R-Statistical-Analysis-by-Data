# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------

mtenstocks <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(mtenstocks)


dim(mtenstocks)


car::some(mtenstocks)



# ----------
# log returns

rtn <- log(mtenstocks[,2:11] + 1)



# ------------------------------------------------------------------------------
# standardize individual series
# ------------------------------------------------------------------------------


std <- diag(1 / sqrt(diag(cov(rtn))))


rtns <- as.matrix(rtn) %*% std




# ------------------------------------------------------------------------------
# Principal component analysis
# ------------------------------------------------------------------------------


m1 <- princomp(rtns)


names(m1)


summary(m1)




# ----------
par(mfrow = c(1,1))

screeplot(m1)



# -->
# we may select PCs = 3 or 4
# the first 3 components exlain about 72.4% of the total variability of the log returns



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
# 1st factor is a weighted average of the log returns with similar weights for stocks in the same industrial sector
# 2nd factor is essentially a weighted difference of the log returns between the semiconductor sector and the
# pharmaceutical industry
# 3rd cactor represents a weighted difference of log returns between the semiconductor sector and the investment banks




# ------------------------------------------------------------------------------
# Covariance matrix of errors
# ------------------------------------------------------------------------------

LLt <- L %*% t(L)


diag(LLt)


sigE <- 1 - diag(LLt)


sigE



# -->
# the variances of the noise components are between 18 to 35% of each standardized return series,
# indicating that marked variablity rimains in each log return series.



# ------------------------------------------------------------------------------
# Estimated commmon factors
# ------------------------------------------------------------------------------


head(m1$scores)


MTSplot(m1$scores[,1:3])





