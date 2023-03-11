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
# Factor Rotation:  Orthogonal rotation  (factors are independent from each other)
#
#   - for orthogonal rotation, we impose restriction on rotaion matrix T:  T T' = I
#     This keeps the orthogonal factor structure intact, the loadings are altered, but fit remains unchanged.
#
#   - "varimax" rotation:  most popular orthogonal rotation
# ------------------------------------------------------------------------------


resFA2 <- fa(polcor, nfactors = 3, rotate = "varimax", fm = "ml")




# ------------------------------------------------------------------------------
# Check factor loadings
# ------------------------------------------------------------------------------

print(resFA$loadings, cutoff = 0.2)


print(resFA2$loadings, cutoff = 0.2)



# -->
# Note that resFA2 is better and match to Princals solution




# ----------
# for comparison
plot(prin)


# MRK, PFE, LLY
# GS, MS, JPM
# TSM, INTC, TXN, MU



# ------------------------------------------------------------------------------
# Communalities
#   - Portion of variances (sum pf squared loadings) of the manifest variables accounted by the common factor effects
#   - squared multiple correlations explained by the common factors
# ------------------------------------------------------------------------------

# note that communality (fit) remain unchanged

round(resFA$communality, 4)

round(resFA2$communality, 4)


