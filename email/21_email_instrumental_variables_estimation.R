setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Instrumental Variable Estimation
# ------------------------------------------------------------------------------


library(AER)


reg.ols <- lm(spend ~ treatment + recency + history + channel, data = biased_data)


reg.iv <- ivreg(spend ~ treatment + recency + history + recency | recency + history + channel, data = biased_data)



stargazer::stargazer(reg.ols, reg.iv, type = "text")


