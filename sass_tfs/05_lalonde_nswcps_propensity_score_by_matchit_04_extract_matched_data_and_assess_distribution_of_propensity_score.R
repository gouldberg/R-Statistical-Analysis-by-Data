setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Create data after matching
# ------------------------------------------------------------------------------


matched_data_nswdw_1 <- match.data(m_mod_nswdw_1)

matched_data_cps1nsw_1 <- match.data(m_mod_cps1nsw_1)

matched_data_cps3nsw_1 <- match.data(m_mod_cps3nsw_1)

matched_data_nswdw_2 <- match.data(m_mod_nswdw_2)

matched_data_cps1nsw_2 <- match.data(m_mod_cps1nsw_2)

matched_data_cps3nsw_2 <- match.data(m_mod_cps3nsw_2)




nrow(matched_data_nswdw_1)


# note that matched_data has only matched control and treated



# ------------------------------------------------------------------------------
# select matched data
# ------------------------------------------------------------------------------

matched_data_list <- list(matched_data_nswdw_1, matched_data_cps1nsw_1, matched_data_cps3nsw_1, matched_data_nswdw_2, matched_data_cps1nsw_2, matched_data_cps3nsw_2)


matched_data_name <- c("matched_data_nswdw_1", "matched_data_cps1nsw_1", "matched_data_cps3nsw_1", "matched_data_nswdw_2", "matched_data_cps1nsw_2", "matched_data_cps3nsw_2")



# select model
matched_data <- matched_data_list[[1]]

matched_data <- matched_data_list[[2]]

matched_data <- matched_data_list[[3]]

matched_data <- matched_data_list[[4]]

matched_data <- matched_data_list[[5]]

matched_data <- matched_data_list[[6]]




# ------------------------------------------------------------------------------
# plot distance (propensity scores) of matched data by covariate 
# ------------------------------------------------------------------------------

graphics.off()



# ----------
# treat
lattice::histogram(~ distance | treat, data = matched_data, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(2,1), xlim = c(0, 1))

plot(m_mod_cps1nsw_1, type = "hist", col = gray(0.7))




# ----------
# black and treat
lattice::histogram(~ distance | black + treat, data = matched_data, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(2,2), xlim = c(0, 1))



# hispanic and treat
lattice::histogram(~ distance | hispanic + treat, data = matched_data, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(2,2), xlim = c(0, 1))





# ------------------------------------------------------------------------------
# AUC and ROC: distance's power to discriminate treat or not  --> this should be close to 0.5
# ------------------------------------------------------------------------------

library(ROCR)


# AUC
performance(prediction(matched_data$distance, matched_data$treat), measure = "auc")@y.values[[1]]




# ----------
par(mfrow = c(1,1))

perf <- performance(prediction(matched_data$distance, matched_data$treat), "tpr", "fpr")

plot(perf, col = "blue")

abline(a = 0, b = 1)




