# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  email
# ------------------------------------------------------------------------------

email <- read_csv("Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

str(email)

dim(email)


car::some(email)




# ------------------------------------------------------------------------------
# Estimate propensity score by logistic regression
# ------------------------------------------------------------------------------


formula1 <- treatment ~ recency + history + channel


ps_model_ori <- glm(data = male_df, formula = formula1, family = binomial)

ps_model_biased <- glm(data = biased_data, formula = formula1, family = binomial)




# ----------
summary(ps_model_ori)

summary(ps_model_biased)


# note that multichannel is reference class
# also that in biased_data, the all coefficients are significant.




# ----------

pred_prob_ori <- predict(ps_model_ori, type = "response")

pred_prob_biased <- predict(ps_model_biased, type = "response")



obj_var <- c("treatment", "recency", "history", "channel")


tmp <- cbind.data.frame(biased_data[,obj_var], pred_prob = pred_prob_biased)




# ----------
graphics.off()

lattice::histogram(~ pred_prob | treatment, data = tmp, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(1,2), xlim = c(0, 1), ylim = c(0, 20))



lattice::histogram(~ pred_prob | channel + treatment, data = tmp, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(3,2), xlim = c(0, 1), ylim = c(0, 25))


# lattice::xyplot(pred_prob ~ history, data = tmp, pch = ".", 
#                col = gray(0.2), xlab = "history", ylab = "propensity score", main = "Propensity Score vs. History")


