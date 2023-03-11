setwd("//media//kswada//MyFiles//R//lalonde_nswcps")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Estimate the effect of "treat" to real earning 78
# Randomized Controlled Trial data
# ------------------------------------------------------------------------------

library(broom)


nsw_cov <- nswdw_data %>%
  lm(data = ., re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married) %>% 
  tidy() %>% filter(term == "treat")



nsw_cov



summary(lm(data = nswdw_data, re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married))



# -->
# the estimated effect is 1676 and statistically significant




# ------------------------------------------------------------------------------
# for reference, not including the covariate
# ------------------------------------------------------------------------------

nsw_cov_ref1 <- nswdw_data %>% lm(data = ., re78 ~ treat) %>%  tidy() %>% filter(term == "treat")

nsw_cov_ref2 <- nswdw_data %>% lm(data = ., re78 ~ treat + education) %>%  tidy() %>% filter(term == "treat")

nsw_cov_ref3 <- nswdw_data %>% lm(data = ., re78 ~ treat + education + black) %>%  tidy() %>% filter(term == "treat")


nsw_cov_ref1

nsw_cov_ref2

nsw_cov_ref3




# -->
# surprisingly, although without full covariate, but the estiamted effect is closing to RCT data
# estiamted effect for treat with only "treat" model is 1794




# ------------------------------------------------------------------------------
# for reference, automatic variable selection
# ------------------------------------------------------------------------------

mod <- lm(data = nswdw_data, re78 ~ treat + re74 + re75 + age + education + black + hispanic + nodegree + married)

# mod <- lm(data = nswdw_data, re78 ~ age + education + black + hispanic + nodegree + married + treat + re74 + re75)



# ----------
mod_update <- step(mod, direction = "both")


summary(mod_update)


tidy(mod_update)



# -->
# variable selection selected "treat", "re74", "education" and "black"
# the estimated effect for "treat" is 1727


