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
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(rtn)


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
# note that communalities for MRK, JPM is under 0.5



# ----------
# here the Princals solution

plot(prin, main = "Loadings")




