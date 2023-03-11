setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Inverse Probability Weighting
# ------------------------------------------------------------------------------


library(WeightIt)


formula1 <- treatment ~ recency + history + channel



# ----------
# estimate weight
# method = "ps":  default, propensity score weighting
# method = "cbps":  covariate balancing propensity score weighting


ipw_mod_ori <- weightit(formula = formula1, data = male_df, method = "ps", estimatnd = "ATE")

ipw_mod_biased <- weightit(formula = formula1, data = biased_data, method = "ps", estimatnd = "ATE")




# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------

ipw_mod_list <- list(ipw_mod_ori, ipw_mod_biased)

ipw_mod_name <- c("ipw_mod_ori", "ipw_mod_biased")



# select model
ipw_mod <- ipw_mod_list[[1]]

ipw_mod <- ipw_mod_list[[2]]




# ----------
summary(ipw_mod)



# ----------
sum(ipw_mod$weights)




# ------------------------------------------------------------------------------
# extract data
# ------------------------------------------------------------------------------

matched_ipw_data_ori <- match.data(ipw_mod_ori)

matched_ipw_data_biased <- match.data(ipw_mod_biased)

