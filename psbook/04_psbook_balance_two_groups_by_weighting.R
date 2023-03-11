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
# Calculate weighting (inverse probability)
# ------------------------------------------------------------------------------
# Weighting for ATE
ps.df$weight_ATE <- with(ps.df, ifelse(TreatmentX == 1, 1 / Propensity_Score, 1 / (1 - Propensity_Score)))


# Weighting for ATT
ps.df$weight_ATT <- with(ps.df, ifelse(TreatmentX == 1, 1, Propensity_Score / (1 - Propensity_Score)))


# Weighting for Stabilized ATE
ps.df$prop_allocation <- mean(ps.df$TreatmentX)

ps.df$coef_ATE <- ifelse(ps.df$TreatmentX == 1, ps.df$prop_allocation, 1 - ps.df$prop_allocation)

ps.df$stabilized_weight_ATE <- ps.df$weight_ATE * ps.df$coef_ATE



# ------------------------------------------------------------------------------
# Compare the distribution of each vars between two groups (TreatmentX == 1, 0)
# ------------------------------------------------------------------------------

library(tableone)

tableone_crude <- CreateTableOne(
  vars = c("Age", "sex", "HT", "DM", "Stroke", "MI"), strata = "TreatmentX", data = ps.df, factorVars = c("sex", "HT", "DM", "Stroke", "MI"))

print(tableone_crude, smd = TRUE, test = FALSE)



# -->
# SMD: standardized mean difference: SMD < 0.1 indicates the balanced



# ------------------------------------------------------------------------------
# Create data taking into account weighting for balancing between 2 groups
# ------------------------------------------------------------------------------

library(survey)

ps_weighted <- svydesign(ids = ~1, data = ps.df, weights = ~ weight_ATE)

tableone_weighted <- svyCreateTableOne(
  vars = c("Age", "sex", "HT", "DM", "Stroke", "MI"), strata = "TreatmentX", data = ps_weighted, factorVars = c("sex", "HT", "DM", "Stroke", "MI"))

print(tableone_weighted, smd = TRUE, test = FALSE)


# -->
# SMD < 0.1 for all variables after weighting.



