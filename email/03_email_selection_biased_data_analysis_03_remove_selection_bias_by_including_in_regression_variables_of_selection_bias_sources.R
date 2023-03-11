setwd("//media//kswada//MyFiles//R//email")

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
# Regression for RCT data and selection-biased data
# ------------------------------------------------------------------------------

library(broom)


# regression for biased_data
biased_reg <- lm(spend ~ treatment + history, data = biased_data)


summary(biased_reg)


( biased_reg_coef <- tidy(biased_reg) )




# ----------
# RCT data
rct_reg <- lm(spend ~ treatment, data = male_df)


summary(rct_reg)


( rct_reg_coef <- tidy(rct_reg) )




# ----------
nonrct_reg <- lm(spend ~ treatment, data = biased_data)


( nonrct_reg_coef <- tidy(nonrct_reg) )




# ----------
# include covariates for source of selection bias
nonrct_mreg <- lm(spend ~ treatment + recency + channel + history, data = biased_data)


nonrct_mreg_coef <- tidy(nonrct_mreg)




# ----------
library(stargazer)

stargazer(biased_reg, rct_reg, nonrct_reg, nonrct_mreg, type = "text")



# -->
# (1):  biased,  treatment + history
# (2):  RCT data,  only treatment
# (3):  biased,  treatment
# (4):  biased,  treatment + recency + channel + history




# ----------
graphics.off()

# car::residualPlots(biased_reg)

# car::residualPlots(rct_reg)

car::residualPlots(nonrct_mreg)

