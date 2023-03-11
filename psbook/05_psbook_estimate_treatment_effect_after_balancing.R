setwd("//media//kswada//MyFiles//R//psbook")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PSbook_data.csv
# ------------------------------------------------------------------------------

ps.df <- read.csv(file = "//media//kswada//MyFiles//references//できる傾向スコア分析//PSbook_data.csv", header = TRUE, sep = ",", fileEncoding = "Shift-JIS")

str(ps.df)
glimpse(ps.df)



# ------------------------------------------------------------------------------
# Estimate Treatment Effect
# ------------------------------------------------------------------------------

library(lmtest)
library(sandwich)

iptw_model <- glm(sequela ~ TreatmentX, family = binomial(link = "logit"), weights = weight_ATE, data = ps.df)

rob_result <- coeftest(iptw_model, vcov = sandwich)

effect <- c(exp(c(rob_result[2], rob_result[2] - 1.96 * rob_result[4], rob_result[2] + 1.96 * rob_result[4])), rob_result[8])

names(effect) <- c("Treatment Effect", "lower 95%CI", "upper 95%CI", "P Value")

effect


# -->
# Odds ratio of TreatmentX is 0.67 (0.55 - 0.82)



# ----------
iptw_model2 <- glm(ADL_disc ~ TreatmentX, family = gaussian(link = "identity"), weights = weight_ATE, data = ps.df)

rob_result2 <- coeftest(iptw_model2, vcov = sandwich)

effect2 <- c(rob_result2[2], rob_result2[2] - 1.96 * rob_result2[4], rob_result2[2] + 1.96 * rob_result2[4], rob_result2[8])

names(effect2) <- c("Treatment Effect", "lower 95%CI", "upper 95%CI", "P Value")

effect2


# -->
# TreatmentX improves ADL_disc by 1.5 (0.78 - 2.2)
