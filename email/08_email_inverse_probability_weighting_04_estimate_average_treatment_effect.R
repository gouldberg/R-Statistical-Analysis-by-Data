setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# model and data list
# ------------------------------------------------------------------------------

ipw_mod_list <- list(ipw_mod_ori, ipw_mod_biased)

matched_ipw_data_list <- list(matched_ipw_data_ori, matched_ipw_data_biased)





# ------------------------------------------------------------------------------
# Estimate Average Treatment Effect
# ------------------------------------------------------------------------------


library(broom)



# ATE: average treatment effect

for(i in 1:length(matched_ipw_data_list)){
  result <- matched_ipw_data_list[[i]] %>% lm(spend ~ treatment, data = ., weights = ipw_mod_list[[i]]$weights) %>% tidy()
  print(result)
}



