setwd("//media//kswada//MyFiles//R//fpc")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fpc  (artifical data set)
# ------------------------------------------------------------------------------

data(fpc)

str(fpc)

dim(fpc)



# ------------------------------------------------------------------------------
# Estimate the mean of x when x > 4
# ------------------------------------------------------------------------------

dfpc <- svydesign(id = ~ psuid, strat = ~ stratid, weight = ~ weight, data = fpc, nest = TRUE)

summary(dfpc)



# ----------
dsub <- subset(dfpc, x > 4)
svymean(~ x, design = dsub)



# ----------
# same operation by svyby
svyby(~ x, ~ I(x > 4), design = dfpc, svymean)



# ----------
# In a regression model with a binary covariate Z and no intercept, 
# there are two coefficients that estimate the mean of the outcome variable in the subpopulations with Z = 0 and Z = 1,
# so we can construct the domain mean estimatorby regression

summary(svyglm(x ~ I(x > 4) + 0, design = dfpc))



# ----------
# Finally, the classical derivation of the domain mean estimator is as a ratiowhere the numerator is X
# for observations in the domain and 0 otherwise andthe denominator is 1 for observations in the domain and 0 otherwise

svyratio(~ I(x * (x > 4)), ~ as.numeric(x > 4), dfpc)

