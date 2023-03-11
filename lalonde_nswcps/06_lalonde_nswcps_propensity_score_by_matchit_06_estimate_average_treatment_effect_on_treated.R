setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# select matched data
# ------------------------------------------------------------------------------

matched_data_list <- list(matched_data_nswdw_1, matched_data_cps1nsw_1, matched_data_cps3nsw_1, matched_data_nswdw_2, matched_data_cps1nsw_2, matched_data_cps3nsw_2)

matched_data_name <- c("matched_data_nswdw_1", "matched_data_cps1nsw_1", "matched_data_cps3nsw_1", "matched_data_nswdw_2", "matched_data_cps1nsw_2", "matched_data_cps3nsw_2")




# ------------------------------------------------------------------------------
# Estimate Average Treatment Effect on Treated (ATT)
# ------------------------------------------------------------------------------


library(broom)



# After matchit, ATT is estimated
# ATT: average treatment effect on the treated

for(i in 1:length(matched_data_list)){
  result <- matched_data_list[[i]] %>% lm(re78 ~ treat, data = .) %>% tidy()
  print(result)
}




summary(lm(data = nswdw_data, re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married))

