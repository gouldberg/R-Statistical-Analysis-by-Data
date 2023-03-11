setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Instrumental Variable Estimation
# ------------------------------------------------------------------------------


library(AER)


formula1 <- re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married

formula2 <- re78 ~ treat + re74 + re75 + I(re74^2) + I(re75^2) + age + education + black + hispanic + nodegree + married



reg.ols <- lm(formula1, data = cps1_nsw_data)


reg.iv <- ivreg(re78 ~ treat + re75 + age + education + nodegree + married | re74 + black + hispanic,
                data = cps1_nsw_data)



stargazer::stargazer(reg.ols, reg.iv, type = "text")


