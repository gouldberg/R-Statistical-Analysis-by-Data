setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# select matched data
# ------------------------------------------------------------------------------

matched_data_list <- list(matched_data_ori, matched_data_biased)

matched_data_name <- c("matched_data_ori", "matched_data_biased")




# ------------------------------------------------------------------------------
# Estimate Average Treatment Effect on the treated
# ------------------------------------------------------------------------------


library(broom)



for(i in 1:length(matched_data_list)){
  result <- matched_data_list[[i]] %>% lm(spend ~ treatment, data = .) %>% tidy()
  print(result)
}


