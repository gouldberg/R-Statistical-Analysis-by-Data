setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Inverse Probability Weighting
# ------------------------------------------------------------------------------


library(WeightIt)


formula1 <- treat ~ re74 + re75 + age + education + black + hispanic + nodegree + married

formula2 <- treat ~ re74 + re75 + I(re74^2) + I(re75^2) + age + education + black + hispanic + nodegree + married



# ----------
# estimate weight
# method = "ps":  default, propensity score weighting
# method = "cbps":  covariate balancing propensity score weighting


ipw_mod_nswdw_1 <- weightit(formula = formula1, data = nswdw_data, method = "ps", estimatnd = "ATE")

ipw_mod_cps1nsw_1 <- weightit(formula = formula1, data = cps1_nsw_data, method = "ps", estimatnd = "ATE")

ipw_mod_cps3nsw_1 <- weightit(formula = formula1, data = cps3_nsw_data, method = "ps", estimatnd = "ATE")


ipw_mod_nswdw_2 <- weightit(formula = formula2, data = nswdw_data, method = "ps", estimatnd = "ATE")

ipw_mod_cps1nsw_2 <- weightit(formula = formula2, data = cps1_nsw_data, method = "ps", estimatnd = "ATE")

ipw_mod_cps3nsw_2 <- weightit(formula = formula2, data = cps3_nsw_data, method = "ps", estimatnd = "ATE")




# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------

ipw_mod_list <- list(ipw_mod_nswdw_1, ipw_mod_cps1nsw_1, ipw_mod_cps3nsw_1, ipw_mod_nswdw_2, ipw_mod_cps1nsw_2, ipw_mod_cps3nsw_2)

ipw_mod_name <- c("ipw_mod_nswdw_1", "ipw_mod_cps1nsw_1", "ipw_mod_cps3nsw_1", "ipw_mod_nswdw_2", "ipw_mod_cps1nsw_2", "ipw_mod_cps3nsw_2")



# select model
ipw_mod <- ipw_mod_list[[1]]

ipw_mod <- ipw_mod_list[[2]]

ipw_mod <- ipw_mod_list[[3]]

ipw_mod <- ipw_mod_list[[4]]

ipw_mod <- ipw_mod_list[[5]]

ipw_mod <- ipw_mod_list[[6]]




# ----------
summary(ipw_mod)



# ----------
sum(ipw_mod$weights)




# ------------------------------------------------------------------------------
# extract data
# ------------------------------------------------------------------------------

matched_ipw_data_nswdw_1 <- match.data(ipw_mod_nswdw_1)

matched_ipw_data_cps1nsw_1 <- match.data(ipw_mod_cps1nsw_1)

matched_ipw_data_cps3nsw_1 <- match.data(ipw_mod_cps3nsw_1)

matched_ipw_data_nswdw_2 <- match.data(ipw_mod_nswdw_2)

matched_ipw_data_cps1nsw_2 <- match.data(ipw_mod_cps1nsw_2)

matched_ipw_data_cps3nsw_2 <- match.data(ipw_mod_cps3nsw_2)

