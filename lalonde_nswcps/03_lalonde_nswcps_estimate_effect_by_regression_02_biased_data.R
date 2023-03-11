setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Estimate the effect of "treat" to real earning 78
# biased data
# ------------------------------------------------------------------------------

# regression with covariates for biased data
cps1_reg <- cps1_nsw_data %>%
  lm(data = ., re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married) %>% 
  tidy() %>% filter(term == "treat")



cps3_reg <- cps3_nsw_data %>%
  lm(data = ., re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married) %>% 
  tidy() %>% filter(term == "treat")



cps1_reg


cps3_reg




# -->
# cps1_reg:  the effect is only 699, less than half of RCT case and NOT statistically significant,
# indicating that the sample from CPS1 is different propensity.
# Or measuring different effect ...


# cps3_reg:  the effect is close to RCT (1676) but less.



# ----------
cps1_reg_s <- lm(data = cps1_nsw_data, re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married)


cps3_reg_s <- lm(data = cps3_nsw_data, re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married)



stargazer::stargazer(cps1_reg_s, cps3_reg_s, type = "text")




# ------------------------------------------------------------------------------
# for reference, not including the covariate
# ------------------------------------------------------------------------------

cps1_reg_ref1 <- cps1_nsw_data %>% lm(data = ., re78 ~ treat) %>% tidy() %>% filter(term == "treat")
cps1_reg_ref2 <- cps1_nsw_data %>% lm(data = ., re78 ~ treat + re74 + re75) %>% tidy() %>% filter(term == "treat")
cps1_reg_ref3 <- cps1_nsw_data %>% lm(data = ., re78 ~ treat + re74 + re75 + education + black) %>% tidy() %>% filter(term == "treat")


cps3_reg_ref1 <- cps3_nsw_data %>% lm(data = ., re78 ~ treat) %>%  tidy() %>% filter(term == "treat")
cps3_reg_ref2 <- cps3_nsw_data %>% lm(data = ., re78 ~ treat + re74 + re75) %>%  tidy() %>% filter(term == "treat")
cps3_reg_ref3 <- cps3_nsw_data %>% lm(data = ., re78 ~ treat + re74 + re75 + education + black) %>%  tidy() %>% filter(term == "treat")


cps1_reg_ref1
cps1_reg_ref2
cps1_reg_ref3

cps3_reg_ref1
cps3_reg_ref2
cps3_reg_ref3



# -->
# not surprisingly different ...
# but by including with covaraites, closing to the effect obtained by RCT data


