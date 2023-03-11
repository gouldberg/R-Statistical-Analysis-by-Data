# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Create data after matching
# ------------------------------------------------------------------------------


matched_data_ori <- match.data(m_mod_ori)

matched_data_biased <- match.data(m_mod_biased)



# ----------
head(matched_data_biased)




# ------------------------------------------------------------------------------
# select matched data
# ------------------------------------------------------------------------------

matched_data_list <- list(matched_data_ori, matched_data_biased)


matched_data_name <- c("matched_data_ori", "matched_data_biased")



# select model
matched_data <- matched_data_list[[1]]

matched_data <- matched_data_list[[2]]





# ------------------------------------------------------------------------------
# plot distance (propensity scores) of matched data by covariate 
# ------------------------------------------------------------------------------

graphics.off()



# ----------
# treat
lattice::histogram(~ distance | treatment, data = matched_data, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(2,1), xlim = c(0, 1))




# ----------
# recency
lattice::histogram(~ distance | recency + treatment, data = matched_data, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(2,2), xlim = c(0, 1))



# history
lattice::histogram(~ distance | cut(history, 5) + treatment, data = matched_data, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(2,2), xlim = c(0, 1))



# channel
lattice::histogram(~ distance | channel + treatment, data = matched_data, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(2,2), xlim = c(0, 1))




# ------------------------------------------------------------------------------
# AUC and ROC: distance's power to discriminate treat or not  --> this should be close to 0.5
# ------------------------------------------------------------------------------

library(ROCR)


# AUC
performance(prediction(matched_data$distance, matched_data$treatment), measure = "auc")@y.values[[1]]




# ----------
par(mfrow = c(1,1))

perf <- performance(prediction(matched_data$distance, matched_data$treatment), "tpr", "fpr")

plot(perf, col = "blue")

abline(a = 0, b = 1)




