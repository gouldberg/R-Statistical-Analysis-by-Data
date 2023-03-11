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
# Estimate treatment effect by adjusting by propensity score
# ------------------------------------------------------------------------------

PS_model2 <- glm(sequela ~ TreatmentX + Propensity_Score, family = binomial(link = "logit"), data = ps.df)

summary(PS_model2)



# ----------
result2 <- as.data.frame(cbind(exp(summary(PS_model2)$coefficient[,1]), exp(confint(PS_model2)), summary(PS_model2)$coefficient[,4]))

names(result2) <- c("Odds ratio", "lower 95%CI", "upper 95%CI", "P value")

result2


# -->
# The odds ratio of Treatment X is 0.71 (0.61 - 0.82)



# ------------------------------------------------------------------------------
# model ADL_disc by TreatmentX and Propensity_Score
# ------------------------------------------------------------------------------

PS_model3 <- glm(ADL_disc ~ TreatmentX + Propensity_Score, family = gaussian(link = "identity"), data = ps.df)

summary(PS_model3)



# ----------
result3 <- as.data.frame(cbind(summary(PS_model3)$coefficient[,1], confint(PS_model3), summary(PS_model3)$coefficients[,4]))

names(result3) <- c("estimates", "lower 95%CI", "upper 95%CI", "P value")

result3


# -->
# TreatmentX improves ADL_disc by 1.5 (1.4 - 1.6)



