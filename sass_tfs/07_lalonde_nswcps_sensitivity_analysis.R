setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Propensity Score matching again by "Matching"
# ------------------------------------------------------------------------------

# we use Matching

library(Matching)



# ----------
# propensity score
data <- nswdw_data
data <- matched_data_nswdw

data <- cps1_nsw_data
data <- matched_data_cps1nsw_1

data <- cps3_nsw_data
data <- matched_data_cps3nsw_1


formula1 <- treat ~ re74 + re75 + age + education + black + hispanic + nodegree + married

formula2 <- treat ~ re74 + re75 + I(re74^2) + I(re75^2) + age + education + black + hispanic + nodegree + married


ps <- glm(formula1, family = binomial, data = data)



# ----------
# propensity score matching

T <- data$treat

O <- data$re78

m <- Match(Y = O, Tr = T, X = ps$fitted, replace = F)

m



# ------------------------------------------------------------------------------
# Sensitivity Analysis
# Rosenbaum's Sensitivity Test for Wilcoxon Signed Rank p-value
# ------------------------------------------------------------------------------

library(rbounds)



# ----------
# Rosenbaum Sensitivity Test for Wilcoxon Signed Rank P-Value 

psens(m, Gamma = 2, GammaInc = 0.1) 



# -->
# for nswdw_data:  at Gamma = 1.0, unconfounded estimate = 0.0047
# for matched_nswdw_data:  at Gamma = 1.0, unconfounded estimate = 0.0021

# for nswdw_data:  but when Gamma is increased to 1.2, the p-values increase to 0.07,
# which suggests a nonsignificant effect.
# This means that if the odds of a person being in the treatment group are only 1.2 times higher
# because of different values on an unobserved covariate,
# even though the other covariates are balanced between the treated and control groups,
# the treatment effect may not be statistically significant.

# In other words, with only a slight increase in selection bias due to an unobserved covarite, the statistical inference would change.



# -->
# for cps3_nsw_data and matched_cps3_nsw_data
# at Gamma = 1.0, unconfounded estimate = 0.3483 and 0.0818  --> propensity score mathing reduced bias
# but Gamma is increased to 1.1, the unconfounded estimate will be increased to 0.15





# ----------
# Rosenbaum Sensitivity Test for Hodges-Lehmann Point Estimate to ATT## 

hlsens(m, Gamma = 2, GammaInc = 0.1, 0.1) 




