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
# Calculate probability for each id to be treated by medicine X  (propensity score)
# ------------------------------------------------------------------------------

# logistic regression
PS_model <- glm(TreatmentX ~ Age + sex + HT + DM + Stroke + MI, family = binomial(link = "logit"), data = ps.df)

summary(PS_model)



# ----------
ps.df$Propensity_Score <- PS_model$fitted.values

head(ps.df)



# ------------------------------------------------------------------------------
# Check c-statistics (= AUC)
# ------------------------------------------------------------------------------
library(pROC)

ROC1 <- roc(TreatmentX ~ Propensity_Score, data = ps.df, ci = TRUE)

ROC1

plot(ROC1)



# ------------------------------------------------------------------------------
# Check overlapping of distributions of propensity score
# ------------------------------------------------------------------------------

TreatX <- subset(ps.df$Propensity_Score, ps.df$TreatmentX == 1)
Control <- subset(ps.df$Propensity_Score, ps.df$TreatmentX == 0)

hist(TreatX, breaks = 20, col = "#9ecae160", border = "#3182bd", xlim = c(0, 1), ylim = c(0, 2100), main = "Distribution of Propensity Score")
hist(Control, breaks = 20, col = "#c994c760", border = "#dd1c77", add = TRUE)
legend("topright", legend = c("TreatmentX", "Control"), col = c("#3182bd", "#dd1c77"), pch = 22)




