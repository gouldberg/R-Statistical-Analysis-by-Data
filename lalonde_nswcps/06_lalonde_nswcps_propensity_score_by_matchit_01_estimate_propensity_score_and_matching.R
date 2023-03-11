setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


library(ROCR)
library(lattice)



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

formula1 <- treat ~ re74 + re75 + age + education + black + hispanic + nodegree + married

formula2 <- treat ~ re74 + re75 + I(re74^2) + I(re75^2) + age + education + black + hispanic + nodegree + married



# ----------
# IT TAKES TIME !!!:  2 - 3 min

m_mod_nswdw_1 <- matchit(formula = formula1, data = nswdw_data, method = "nearest", replace = TRUE)

m_mod_cps1nsw_1 <- matchit(formula = formula1, data = cps1_nsw_data, method = "nearest", replace = TRUE)

m_mod_cps3nsw_1 <- matchit(formula = formula1, data = cps3_nsw_data, method = "nearest", replace = TRUE)

m_mod_nswdw_2 <- matchit(formula = formula2, data = nswdw_data, method = "nearest", replace = TRUE)

m_mod_cps1nsw_2 <- matchit(formula = formula2, data = cps1_nsw_data, method = "nearest", replace = TRUE)

m_mod_cps3nsw_2 <- matchit(formula = formula2, data = cps3_nsw_data, method = "nearest", replace = TRUE)




# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------

m_mod_list <- list(m_mod_nswdw_1, m_mod_cps1nsw_1, m_mod_cps3nsw_1, m_mod_nswdw_2, m_mod_cps1nsw_2, m_mod_cps3nsw_2)

m_mod_name <- c("m_mod_nswdw_1", "m_mod_cps1nsw_1", "m_mod_cps3nsw_1", "m_mod_nswdw_2", "m_mod_cps1nsw_2", "m_mod_cps3nsw_2")



# select model
m_mod <- m_mod_list[[1]]

m_mod <- m_mod_list[[2]]

m_mod <- m_mod_list[[3]]

m_mod <- m_mod_list[[4]]

m_mod <- m_mod_list[[5]]

m_mod <- m_mod_list[[6]]




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


