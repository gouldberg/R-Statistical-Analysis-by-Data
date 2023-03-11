setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# model and data list
# ------------------------------------------------------------------------------

ipw_mod_list <- list(ipw_mod_nswdw_1, ipw_mod_cps1nsw_1, ipw_mod_cps3nsw_1, ipw_mod_nswdw_2, ipw_mod_cps1nsw_2, ipw_mod_cps3nsw_2)

matched_ipw_data_list <- list(matched_ipw_data_nswdw_1, matched_ipw_data_cps1nsw_1, matched_ipw_data_cps3nsw_1, matched_ipw_data_nswdw_2, matched_ipw_data_cps1nsw_2, matched_ipw_data_cps3nsw_2)





# ------------------------------------------------------------------------------
# Estimate Average Treatment Effect
# ------------------------------------------------------------------------------


library(broom)



# ATE: average treatment effect

for(i in 1:length(matched_ipw_data_list)){
  result <- matched_ipw_data_list[[i]] %>% lm(re78 ~ treat, data = ., weights = ipw_mod_list[[i]]$weights) %>% tidy()
  print(result)
}




summary(lm(data = nswdw_data, re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married))





# -->
# intercept = E[Y0] = 14735
# treatment = E[Y1] - E[Y0] = -7627


# Although this model is statistically significant,
# but if difference of propensity among controlled and treated group, IPW's result is not RELIABLE !!!

# In CPS1 (no treatment), there are some group with large real earnings, for whom the NSW is no required,
# and E[Y0] is estimated as large value ...

