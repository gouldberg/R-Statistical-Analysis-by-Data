setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# select data
# ------------------------------------------------------------------------------

matched_ipw_data_list <- list(matched_ipw_data_nswdw_1, matched_ipw_data_cps1nsw_1, matched_ipw_data_cps3nsw_1, 
                              matched_ipw_data_nswdw_2, matched_ipw_data_cps1nsw_2, matched_ipw_data_cps3nsw_2)



# select data

matched_ipw_data <- matched_ipw_data_list[[1]]

matched_ipw_data <- matched_ipw_data_list[[2]]

matched_ipw_data <- matched_ipw_data_list[[3]]

matched_ipw_data <- matched_ipw_data_list[[4]]

matched_ipw_data <- matched_ipw_data_list[[5]]

matched_ipw_data <- matched_ipw_data_list[[6]]




# ------------------------------------------------------------------------------
# assess the weights distribution by covariate
# ------------------------------------------------------------------------------

graphics.off()



# ----------
# treat
lattice::histogram(~ weights | treat, data = matched_ipw_data, 
                   xlab = "weights", 
                   layout = c(1,2))





# ----------
# black and treat

lattice::histogram(~ weights | black + treat, data = matched_ipw_data, 
                   xlab = "weights", 
                   layout = c(1,2))


summary(matched_ipw_data %>% filter(treat == 1, black == 1) %>% dplyr::select(weights) %>% pull)

summary(matched_ipw_data %>% filter(treat == 0, black == 1) %>% dplyr::select(weights) %>% pull)



# -->
# There are large weight for treat == 1 and black == 1 




# hispanic and treat
lattice::histogram(~ weights | hispanic + treat, data = matched_ipw_data, 
                   xlab = "weights", 
                   layout = c(1,2))



