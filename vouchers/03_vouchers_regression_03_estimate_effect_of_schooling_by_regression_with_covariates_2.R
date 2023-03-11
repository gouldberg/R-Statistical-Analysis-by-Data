setwd("//media//kswada//MyFiles//R//vouchers")

packages <- c("dplyr", "tidyverse", "broom")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  vauchers
# ------------------------------------------------------------------------------

vouchers <- read_tsv("vouchers.txt")

str(vouchers)

dim(vouchers)


car::some(vouchers)



# ------------------------------------------------------------------------------
# Different effect by gender ?
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Angrist (2002) Table 4 and 6t bogota 1995 table
# ------------------------------------------------------------------------------

data_tbl4_bog95 <- vouchers %>%
  filter(BOG95SMP == 1, TAB3SMPL == 1,
         !is.na(SCYFNSH), !is.na(FINISH6), !is.na(PRSCHA_1),
         !is.na(REPT6), !is.na(NREPT), !is.na(INSCHL),
         !is.na(FINISH7),
         !is.na(PRSCH_C), !is.na(FINISH8), !is.na(PRSCHA_2),
         !is.na(TOTSCYRS), !is.na(REPT)
  ) %>%
  select(VOUCH0, SVY, HSVISIT, DJAMUNDI, PHONE, AGE,
         STRATA1:STRATA6, STRATAMS, DBOGOTA, D1993, D1995, D1997,
         DMONTH1:DMONTH12, SEX_MISS, FINISH6, FINISH7, FINISH8,
         REPT6, REPT, NREPT, SEX2, TOTSCYRS, MARRIED, HASCHILD,
         HOURSUM,WORKING3, INSCHL,PRSCH_C,USNGSCH,PRSCHA_1)


data_tbl4_bog95




# ------------------------------------------------------------------------------
# Regression for female and male, separately
# ------------------------------------------------------------------------------

# female
regression_data <- data_tbl4_bog95 %>% filter(SEX2 == 0)


df_models <- models %>%
  mutate(model = map(.x = formula, .f = lm, data = regression_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))


df_results_female <- df_models %>% mutate(formula = as.character(formula), gender = "female") %>%
  select(formula, model_index, lm_result, gender) %>%
  unnest(cols = c(lm_result))


df_results_female



# ----------
# male
regression_data <- data_tbl4_bog95 %>% filter(SEX2 == 1)


df_models <- models %>%
  mutate(model = map(.x = formula, .f = lm, data = regression_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))


df_results_male <- df_models %>% mutate(formula = as.character(formula), gender = "male") %>%
  select(formula, model_index, lm_result, gender) %>%
  unnest(cols = c(lm_result))


df_results_male



# ------------------------------------------------------------------------------
# 通学傾向への分析結果の可視化
# effect for PRSCHA_1, USNGSCH
# ------------------------------------------------------------------------------

using_voucher_results_gender <- rbind(df_results_male, df_results_female) %>%
  filter(term == "VOUCH0", str_detect(model_index, "PRSCHA_1|USNGSCH")) %>%
  select(gender, model_index, term, estimate, std.error, p.value) %>%
  arrange(gender, model_index) %>%
  filter(str_detect(model_index, "covariate"))



using_voucher_results_gender %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(gender ~ .)



# ------------------------------------------------------------------------------
# 留年と通学年数への分析結果の可視化
# effect for PRSCH_C, INSCHL, REPT, TOTSCYRS, FINISH
# ------------------------------------------------------------------------------

going_private_results_gender <- rbind(df_results_male, df_results_female) %>%
  filter(term == "VOUCH0",
         str_detect(model_index, "PRSCH_C|INSCHL|REPT|TOTSCYRS|FINISH")) %>%
  select(gender, model_index, term, estimate, std.error, p.value) %>%
  arrange(model_index)


going_private_results_gender %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(gender ~ .)




# ------------------------------------------------------------------------------
# 労働時間に対する分析結果の可視化
# effect for HOUR
# ------------------------------------------------------------------------------

working_hour_results_gender <- rbind(df_results_male, df_results_female) %>%
  filter(term == "VOUCH0", str_detect(model_index, "HOUR")) %>%
  select(gender, model_index, term, estimate, std.error, p.value) %>%
  arrange(gender, model_index)


working_hour_results_gender %>%
  filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm")) +
  facet_grid(. ~ gender)


