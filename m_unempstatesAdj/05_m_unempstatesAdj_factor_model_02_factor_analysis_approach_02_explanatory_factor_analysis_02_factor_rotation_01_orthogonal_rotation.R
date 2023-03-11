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





# ----------
# for comparison
plot(prin2)




# ------------------------------------------------------------------------------
# Communalities
#   - Portion of variances (sum pf squared loadings) of the manifest variables accounted by the common factor effects
#   - squared multiple correlations explained by the common factors
# ------------------------------------------------------------------------------

# note that communality (fit) remain unchanged

round(resFA$communality, 4)

round(resFA2$communality, 4)


