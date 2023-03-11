setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# select matched data
# ------------------------------------------------------------------------------

matched_data_list <- list(matched_data_ori, matched_data_biased)

matched_data_name <- c("matched_data_ori", "matched_data_biased")




# ------------------------------------------------------------------------------
# Estimate Average Treatment Effect (ATE)
# ------------------------------------------------------------------------------


library(broom)



# Note that weights are applied

for(i in 1:length(matched_data_list)){
  ratio <- sum(matched_data_list[[i]]$treat == 1) / nrow(matched_data_list[[i]])
  # result <- matched_data_list[[i]] %>% lm(spend ~ treatment, data = ., weights = (treat / distance) + ((1 - treat) / (1 - distance))) %>% tidy()
  result <- matched_data_list[[i]] %>% lm(spend ~ treatment, data = ., weights = (ratio / distance) + ((1 - ratio) / (1 - distance))) %>% tidy()
  print(result)
}



