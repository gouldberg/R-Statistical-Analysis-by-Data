setwd("//media//kswada//MyFiles//R//njmin3")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Estimate Average Treatment Effect on Treated (ATT)
# ------------------------------------------------------------------------------

library(broom)


PSM_result1 <- matched_data1 %>% lm(fte ~ nj * d, data = .) %>% tidy()
PSM_result2 <- matched_data2 %>% lm(fte ~ nj * d, data = .) %>% tidy()
PSM_result3 <- matched_data3 %>% lm(fte ~ nj * d, data = .) %>% tidy()
PSM_result4 <- matched_data4 %>% lm(fte ~ nj * d, data = .) %>% tidy()
PSM_result5 <- matched_data5 %>% lm(fte ~ nj * d, data = .) %>% tidy()
PSM_result6 <- matched_data6 %>% lm(fte ~ nj * d, data = .) %>% tidy()



# After matchit, ATT is estimated
PSM_result1
PSM_result2
PSM_result3
PSM_result4
PSM_result5
PSM_result6



# -->
# here, effect is 2.60 or less
# (by Difference-in-Differences approach:  2.75, 2.85, 2.81)


