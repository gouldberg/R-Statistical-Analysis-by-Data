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
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(drate)


polcor




# ------------------------------------------------------------------------------
# Explanatory Factor Analaysis
# ------------------------------------------------------------------------------

library(psych)


resFA <- fa(polcor, nfactors = 3, rotate = "none", fm = "ml")


summary(resFA)




# ------------------------------------------------------------------------------
# Check factor loadings
# ------------------------------------------------------------------------------

print(resFA$loadings)




# ----------
# blanking out lodings < 0.2

print(resFA$loadings, cutoff = 0.2)




# ------------------------------------------------------------------------------
# Communalities
#   - Portion of variances (sum pf squared loadings) of the manifest variables accounted by the common factor effects
#   - squared multiple correlations explained by the common factors
# ------------------------------------------------------------------------------


round(resFA$communality, 2)



# -->
# note that communalities for Arkansas (AK) is only 0.19



# ----------
# here the Princals solution

plot(prin2, main = "Loadings")




