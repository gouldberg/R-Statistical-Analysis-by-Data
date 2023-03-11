setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Estimate propensity score by logistic regression for biased data
# ------------------------------------------------------------------------------


formula1 <- treat ~ re74 + re75 + age + education + black + hispanic + nodegree + married

formula2 <- treat ~ re74 + re75 + I(re74^2) + I(re75^2) + age + education + black + hispanic + nodegree + married




# ----------
# formula1
ps_model_nswdw_1 <- glm(data = nswdw_data, formula = formula1, family = binomial)

ps_model_cps1nsw_1 <- glm(data = cps1_nsw_data, formula = formula1, family = binomial)

ps_model_cps3nsw_1 <- glm(data = cps3_nsw_data, formula = formula1, family = binomial)




# ----------
ps_model_nswdw_2 <- glm(data = nswdw_data, formula = formula2, family = binomial)

ps_model_cps1nsw_2 <- glm(data = cps1_nsw_data, formula = formula2, family = binomial)

ps_model_cps3nsw_2 <- glm(data = cps3_nsw_data, formula = formula2, family = binomial)




# ----------

summary(ps_model_nswdw_1)

summary(ps_model_cps1nsw_1)

summary(ps_model_cps3nsw_1)



# -->
# note that in ps_model_cps1nsw_1, not only black but also hispanic and others are highly significant
# note that in ps_model_cps3nsw_1, black is really highly significant, but hispanic is marginally significant (p.value = 0.02)





# ----------
pred_prob_nswdw_1 <- predict(ps_model_nswdw_1, type = "response")

pred_prob_cps1nsw_1 <- predict(ps_model_cps1nsw_1, type = "response")

pred_prob_cps3nsw_1 <- predict(ps_model_cps3nsw_1, type = "response")

pred_prob_nswdw_2 <- predict(ps_model_nswdw_2, type = "response")

pred_prob_cps1nsw_2 <- predict(ps_model_cps1nsw_2, type = "response")

pred_prob_cps3nsw_2 <- predict(ps_model_cps3nsw_2, type = "response")




obj_var <- c("treat", "re74", "re75", "age", "education", "black", "hispanic", "nodegree", "married")

tmp <- cbind.data.frame(cps1_nsw_data[,obj_var], pred_prob = pred_prob_cps1nsw_1)




# ----------
graphics.off()

lattice::histogram(~ pred_prob | treat, data = tmp, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(1,2), xlim = c(0, 1), ylim = c(0, 20))



lattice::histogram(~ pred_prob | black + treat, data = tmp, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(2,2), xlim = c(0, 1), ylim = c(0, 25))


lattice::histogram(~ pred_prob | hispanic + treat, data = tmp, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(2,2), xlim = c(0, 1), ylim = c(0, 25))



