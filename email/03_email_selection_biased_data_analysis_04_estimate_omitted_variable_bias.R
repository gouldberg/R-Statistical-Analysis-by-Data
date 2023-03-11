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
# Estimate Omitted Variable Bias
# ------------------------------------------------------------------------------

# Not including "history"
short_coef <- biased_data %>% lm(data = ., formula = spend ~ treatment + recency + channel) %>% tidy()


alpha_1 <- short_coef %>% filter(term == "treatment") %>% pull(estimate)



# ----------
# Including "history"
long_coef <- biased_data %>% lm(data = ., formula = spend ~ treatment + recency + channel + history) %>% tidy()


beta_1 <- long_coef %>% filter(term == "treatment") %>% pull(estimate)

beta_2 <- long_coef %>% filter(term == "history") %>% pull(estimate)



# ----------
# Regress omitted variable ("history") on variable of selection bias sources
omitted_coef <- biased_data %>%
  lm(data = ., formula = history ~ treatment + channel + recency) %>%
  tidy()


gamma_1 <- omitted_coef %>% filter(term == "treatment") %>% pull(estimate)




# ----------
# Omitted Variable Bias
beta_2 * gamma_1

alpha_1 - beta_1




# ------------------------------------------------------------------------------
# Estimate Omitted Variable Bias by broom
# ------------------------------------------------------------------------------

library(broom)


formula_vec <- c(spend ~ treatment + recency + channel,
                 spend ~ treatment + recency + channel + history,
                 history ~ treatment + channel + recency)


names(formula_vec) <- paste("reg", LETTERS[1:3], sep ="_")


models = formula_vec %>%
  enframe(name = "model_index", value = "formula")


models



# ----------
df_models <- models %>%
  mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))



df_models




# ----------

df_results <- df_models %>%
  mutate(formula = as.character(formula)) %>%
  select(formula, model_index, lm_result) %>%
  unnest(cols = lm_result)


df_results


treatment_coef <- df_results %>% filter(term == "treatment") %>% pull(estimate)

history_coef <- df_results %>% filter(model_index == "reg_B", term == "history") %>% pull(estimate)




# ----------
# Omitted Variable Bias

OVB <- history_coef * treatment_coef[3]

coef_gap <- treatment_coef[1] - treatment_coef[2]



OVB

beta_2 * gamma_1


coef_gap

alpha_1 - beta_1

