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
# Estimate Post-treatment Bias:
#  - Correlation of "visit" to treatment
# ------------------------------------------------------------------------------

library(broom)


# include "visit", which occurred "after" treatment:
# mailing --> more visits --> more spend


# regress treatment
cor_visit_treatment <- lm(data = biased_data, formula = treatment ~ visit + channel + recency + history) %>% tidy()


cor_visit_treatment




# ------------------------------------------------------------------------------
# Estimate Post-treatment Bias:
#  - include "visit" and regress spend
# ------------------------------------------------------------------------------


bad_control_reg <- lm(data = biased_data, formula = spend ~ treatment + channel + recency + history + visit) %>% tidy()


bad_control_reg



# -->
# the coefficient of treatment is decreased to 0.294
# since as for visitors, the lower propensity of purchase is dominant

# this is "post treatment bias"


# -->
# So the vairable which occurs after treatment ("mailing") should be excluded from model



