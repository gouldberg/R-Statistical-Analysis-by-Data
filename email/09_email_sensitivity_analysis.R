setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Propensity Score matching again by "Matching"
# ------------------------------------------------------------------------------

# we use Matching

library(Matching)



# ----------
# propensity score
data <- male_df
data <- biased_data

data <- matched_data_ori
data <- matched_data_biased



formula1 <- treatment ~ recency + history + channel


ps <- glm(formula1, family = binomial, data = data)



# ----------
# propensity score matching

T <- data$treatment

O <- data$spend

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






# ----------
# Rosenbaum Sensitivity Test for Hodges-Lehmann Point Estimate to ATT## 

hlsens(m, Gamma = 2, GammaInc = 0.1, 0.1) 




