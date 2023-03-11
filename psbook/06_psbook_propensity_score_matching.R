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
# Propensity score matching
# ------------------------------------------------------------------------------

library(Matching)

set.seed(123)


# ----------
# caliper = 0.2: standard deviation of Propensity_Score * 0.2 is set to caliper
# replace = FALSE:  sampling without replacement
match_result <- Match(Tr = ps.df$TreatmentX, X = ps.df$Propensity_Score, M = 1, caliper = 0.2, replace = FALSE)

summary(match_result)

match_result



# ----------
ps.df_matched <- rbind(ps.df[match_result$index.treated,], ps.df[match_result$index.control,])

head(ps.df_matched)



# ----------
TreatX_match <- subset(ps.df_matched$Propensity_Score, ps.df_matched$TreatmentX == 1)

Control_match <- subset(ps.df_matched$Propensity_Score, ps.df_matched$TreatmentX == 0)

hist(TreatX_match, breaks = 20, col = "#9ecae160", border = "#3182bd", xlim = c(0, 1), ylim = c(0, 500), main = "Distribution of Propensity Score")
hist(Control_match, breaks = 20, col = "#c994c760", border = "#dd1c77", add = TRUE)

legend("topright", legend = c("TreatmentX", "Control"), col = c("#3182bd", "#dd1c77"), pch = 22)


# -->
# Distribution of propensity score is overlapped.



# ------------------------------------------------------------------------------
# Compare matched 2 groups
# ------------------------------------------------------------------------------

tableone_matched <- CreateTableOne(
  vars = c("Age", "sex", "HT", "DM", "Stroke", "MI"), strata = "TreatmentX", data = ps.df_matched, factorVars = c("sex", "HT", "DM", "Stroke", "MI"), test = FALSE)

print(tableone_matched, smd = TRUE)


# -->
# SMD of all variables < 0.1, indicating the distributions are balanced.



# ------------------------------------------------------------------------------
# Estimate Treatment Effect
# ------------------------------------------------------------------------------

tableone_result_matched <- CreateTableOne(
  vars = c("sequela", "ADL_disc"), strata = "TreatmentX", data = ps.df_matched, factorVars = "sequela", test = TRUE)

print(tableone_result_matched)


# -->
# sequela is 9.6% and 6.8% for TreatmentX == 1 and 0, respectively. (statistically significant)
# Also, ADL_disc is 39.93 and 41.34



