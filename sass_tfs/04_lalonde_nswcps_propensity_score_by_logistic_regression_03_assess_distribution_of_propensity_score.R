setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


library(ROCR)
library(lattice)



# ------------------------------------------------------------------------------
# select model
# ------------------------------------------------------------------------------


mod_list <- list(ps_model_nswdw_1, ps_model_cps1nsw_1, ps_model_cps3nsw_1, ps_model_nswdw_2, ps_model_cps1nsw_2, ps_model_cps3nsw_2)

mod_name <- c("ps_model_nswdw_1", "ps_model_cps1nsw_1", "ps_model_cps3nsw_1", "ps_model_nswdw_2", "ps_model_cps1nsw_2", "ps_model_cps3nsw_2")



# select model
mod <- mod_list[[1]]

mod <- mod_list[[2]]

mod <- mod_list[[3]]

mod <- mod_list[[4]]

mod <- mod_list[[5]]

mod <- mod_list[[6]]



# ----------
pred_prob <- predict(mod, type = "response")




# ------------------------------------------------------------------------------
# summary statistics
# ------------------------------------------------------------------------------


# propensity scores
by(pred_prob, mod$data$treat, summary)



# covariates
by(mod$data[,c("black", "hispanic")], mod$data$treat, summary)




# ------------------------------------------------------------------------------
# AUC and ROC
# ------------------------------------------------------------------------------

# AUC
performance(prediction(pred_prob, mod$data$treat), measure = "auc")@y.values[[1]]



# ----------
par(mfrow = c(1,1))

perf <- performance(prediction(pred_prob, mod$data$treat), "tpr", "fpr")

plot(perf, col = "blue")

abline(a = 0, b = 1)




# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# ------------------------------------------------------------------------------

# obtain proportion of treated cases above maximum control cases and proportion of control cases below minum treated cases

idx_0 <- which(mod$data$treat == 0)

idx_1 <- which(mod$data$treat == 1)


100 * c(
  mean(as.numeric(pred_prob[idx_1] > max(pred_prob[idx_0]))),
  mean(as.numeric(pred_prob[idx_0] < min(pred_prob[idx_1])))
)



# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# Box-and-Whiskers plot
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

boxplot(pred_prob ~ mod$data$treat, ylab = "Propensity Scores", xlab = "Treatment", main = "by logistic regression", ylim = c(0, 1))




# ------------------------------------------------------------------------------
# Evaluate the common support for the distribution of propensity scores of treated and untreated groups
# Kernel density plots
# ------------------------------------------------------------------------------


densityplot( ~ pred_prob, groups=mod$data$treat, plot.points = T, col = c(gray(0.5), "black"), lty = c(2,1),
             xlim=c(0,1), lwd = 2,
              ylab = "Propensity Scores", xlab = "Treatment")


histogram( ~ pred_prob | mod$data$treat, plot.points = T,
             xlim = c(0,1), layout = c(2,1),
             ylab = "Propensity Scores", xlab = "Treatment")



