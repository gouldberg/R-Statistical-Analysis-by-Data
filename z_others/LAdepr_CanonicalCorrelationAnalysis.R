# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  LAdepr
#  - data from a study of depression amongst 294 respondents in Los Angeles
#  - CESD:  health variables, the sum of 20 separate numerical scales measuring different aspects of depression
#  - Health:  a measure of general health
#  - personal variables:  gender, age, income, and educational level (numerically coded from the lowest "less than high school", to the highest, "finished doctrate")
# ------------------------------------------------------------------------------

# Sample correlation matrix between variables
CESD <- c(1.000, 0.212, 0.124, -0.164, -0.101, -0.158)
Health <- c(0.212, 1.000, 0.098, 0.308, -0.207, -0.183)
Gender <- c(0.124, 0.098, 1.000, 0.044, -0.106, -0.180)
Age <- c(-0.164, 0.308, 0.044, 1.000, -0.208, -0.192)
Edu <- c(-0.101, -0.207, -0.106, -0.208, 1.000, 0.492)
Income <- c(-0.158, -0.183, -0.180, -0.192, 0.492, 1.000)

cormat <- cbind(CESD, Health, Gender, Age, Edu, Income)

row.names(cormat) <- c("CESD", "Health", "Gender", "Age", "Edu", "Income")
colnames(cormat) <- c("CESD", "Health", "Gender", "Age", "Edu", "Income")

cormat


# ------------------------------------------------------------------------------
# canonical correlations analysis
#  - canonical correlation and corresponding canonical variates (loadings)
# ------------------------------------------------------------------------------
# canonical correlation between response variables(Y: CESD, personal) and explanatory variables(X: personal variables)
cormat


Y <- 1:2
X <- 3:6

( ryy <- cormat[Y, Y] )
( rxx <- cormat[X, X] )
( ryx <- cormat[Y, X] )
( rxy <- cormat[X, Y] )


( E1 <- solve(ryy) %*% ryx %*% solve(rxx) %*% rxy )
( E2 <- solve(rxx) %*% rxy %*% solve(ryy) %*% ryx )


# eigen values and loadings
( e1 <- eigen(E1) )
( e2 <- eigen(E2) )


# -----------------------------------
# The 1st canonical correlation is 0.409
sqrt(e1$values[1])

# Corresponding canonical variates (loadings) are
#  u1 = 0.53 * CESD - 0.85 * Health
#  v1 = - 0.0026 * Gender - 0.98 * Age + 0.19 * Edu - 0.07 Income
# relatively older and medicated people tend to have lower depression scores,
# but perceive their health as relatively poor, while relatively younger but educated people have the opposite health perception
e1$vectors[,1]
e2$vectors[,1]


# -----------------------------------
# 2nd canonical correlation is
sqrt(e1$values[2])

# Corresponding canonical variates (loadings) are
#  u2 = 0.53 * CESD - 0.42 * Health
#  v2 = 0.49 * Gender - 0.32 * Age - 0.43 * Edu - 0.69 Income
# relatively young, poor, and uneducated females are associated with higher depression scores,
# and, to a lesser extent, with poor perceived health
e1$vectors[,2]
e2$vectors[,2]


