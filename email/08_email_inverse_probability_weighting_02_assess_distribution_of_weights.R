setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# select data
# ------------------------------------------------------------------------------

matched_ipw_data_list <- list(matched_ipw_data_ori, matched_ipw_data_biased)



# select data

matched_ipw_data <- matched_ipw_data_list[[1]]

matched_ipw_data <- matched_ipw_data_list[[2]]




# ------------------------------------------------------------------------------
# assess the weights distribution by covariate
# ------------------------------------------------------------------------------

graphics.off()



# ----------
# treat
lattice::histogram(~ weights | treatment, data = matched_ipw_data, 
                   xlab = "weights", 
                   layout = c(1,2))





# ----------
# recency and treat

lattice::histogram(~ weights | recency + treatment, data = matched_ipw_data, 
                   xlab = "weights", 
                   layout = c(1,2))





# channel and treat
lattice::histogram(~ weights | channel + treatment, data = matched_ipw_data, 
                   xlab = "weights", 
                   layout = c(1,2))



