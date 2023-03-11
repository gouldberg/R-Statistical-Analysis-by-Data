# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Propensity Score Matching
# ------------------------------------------------------------------------------

library(MatchIt)


# need to set seed
set.seed(12345)



# ----------
# Matching by propensity score
# including variables of selection-bias sources
# method = "near":  nearest neighbor matching
# default distance measure is "logit" (logistic regression)
# replace = TRUE:  each control unit can be matched only once

formula1 <- treatment ~ recency + history + channel



# ----------
# IT TAKES TIME !!!:  2 - 3 min

m_mod_ori <- matchit(formula = formula1, data = male_df, method = "nearest", replace = TRUE)

m_mod_biased <- matchit(formula = formula1, data = biased_data, method = "nearest", replace = TRUE)




# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------

m_mod_list <- list(m_mod_ori, m_mod_biased)

m_mod_name <- c("m_mod_ori", "m_mod_biased")



# select model
m_mod <- m_mod_list[[1]]

m_mod <- m_mod_list[[2]]




# ----------
summary(m_mod)




# ----------
# basic summary table of matched data
m_mod$nn



# estimated distance measure for each unit (estimated propensity scores)
m_mod$distance

length(m_mod$distance)




# ------------------------------------------------------------------------------
# Propensity Score Matching:  weights
# ------------------------------------------------------------------------------

# weghts assigned to each unit in the matching process.

sum(m_mod$weights)


# Matched Treated Units have weight 1
sum(m_mod$weights == 1)


# Unmatched units have weights equal to zero.
sum(m_mod$weights == 0)


# Each matched control unit has weight proportional to the number of treatment units to which it was matched,
sum(! m_mod$weights %in% c(0, 1))


sum(m_mod$weights[! m_mod$weights %in% c(0, 1)])


# the units ineligible for matching due to common support restrictions
m_mod$discarded[m_mod$discarded == TRUE]


