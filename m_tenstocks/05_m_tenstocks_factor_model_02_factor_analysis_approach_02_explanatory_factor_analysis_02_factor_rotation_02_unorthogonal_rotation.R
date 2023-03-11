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
# Factor Rotation:  Un-Orthogonal rotaion ("oblique rotation")  (factors are NOT independent from each other)
#
#   - for un-orthogonal rotation, not any more rotation matrix T:  T T' != I
#
#   - Popular un-orthogonal ("oblique") rotation approaches:  "oblimin" and "promax"
#   - In practive, EFA with oblique rotation is often used prior to a CFA in order to explore
#     whether the underlying latent structure theory is reflected by the data.
# ------------------------------------------------------------------------------


resFA3 <- fa(polcor, nfactors = 3, rotate = "oblimin", fm = "ml")


resFA4 <- fa(polcor, nfactors = 4, rotate = "oblimin", fm = "ml")




# ------------------------------------------------------------------------------
# Check factor loadings
# ------------------------------------------------------------------------------

print(resFA3$loadings, cutoff = 0.2)


print(resFA4$loadings, cutoff = 0.2)



# -->
# Unorthogonal solutions are better match to Princal solutions !!


# ----------
# for comparison
plot(prin)


# MRK, PFE, LLY
# GS, MS, JPM
# TSM, INTC, TXN, MU



# ----------
fa.diagram(resFA2)

fa.diagram(resFA3)

fa.diagram(resFA4)




# ------------------------------------------------------------------------------
# Communalities
#   - Portion of variances (sum pf squared loadings) of the manifest variables accounted by the common factor effects
#   - squared multiple correlations explained by the common factors
# ------------------------------------------------------------------------------

# note that communality (fit) remain unchanged for 2 factors but changed for 3 factors

round(resFA2$communality, 4)

round(resFA3$communality, 4)

round(resFA4$communality, 4)



# -->
# for JPM, increased to 0.5864
# for PFE, increased to 0.6913
# but for MRK, decreased to 0.4189



# ------------------------------------------------------------------------------
# Factor correlation matrix
# ------------------------------------------------------------------------------

# since the rotation is un-orthogonal, factors are correlated

round(resFA3$Phi, 3)

round(resFA4$Phi, 4)



# -->
# factors are fairly high correlated with each other ...






