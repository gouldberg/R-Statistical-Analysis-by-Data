setwd("//media//kswada//MyFiles//R//snow")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SNOW
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# Difference in Differences by company
# Use dummy variable for company and year
# ------------------------------------------------------------------------------

library(broom)


# Difference in Differences for company data
# Note that JS_sum is only 4 * 4 data frame

JS_did <- JS_sum %>%
  mutate(D1854 = if_else(year == 1854, 1, 0)) %>%
  lm(data = ., death ~ LSV + D1854 + D1854:LSV) %>%
  tidy()



# in log (to focus on the change in rate)
JS_did_log <- JS_sum %>%
  mutate(D1854 = if_else(year == 1854, 1, 0)) %>%
  lm(data = ., log(death) ~ LSV + D1854 + D1854:LSV) %>%
  tidy()


JS_did

JS_did_log



# -->
# The number of died was indeed decreased in 1854 by 51%

# This data has only 4 samples, so the standard errors are not calculated.



# ------------------------------------------------------------------------------
# Difference in Differences by area
# ------------------------------------------------------------------------------


# Difference in Differences for area data 

JS_did_area <- JS_df %>%
  mutate(D1854 = if_else(year == 1854, 1, 0)) %>%
  lm(data = ., death ~ LSV + area + D1854 + D1854:LSV) %>%
  tidy() %>%
  filter(!str_detect(term, "area"))



# in log (to focus on the change in rate)
JS_did_area_log <- JS_df %>%
  mutate(D1854 = if_else(year == 1854, 1, 0)) %>%
  lm(data = ., log(death) ~ LSV + area + D1854 + D1854:LSV) %>%
  tidy() %>%
  filter(!str_detect(term, "area"))



JS_did_area

JS_did_area_log



# -->
# Also by area data,
# the number of died was indeed decreased in 1854 by 56.6%
# (but the coefficient for non-log case is small due to scale)
