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
# 通学率と奨学金の利用傾向の可視化
# effect of VOUCH0 for PRSCHA_1, USNGSCH  (no drop-off schooling in private and non-private school ?)
# ------------------------------------------------------------------------------

using_voucher_results <- df_results %>% filter(term == "VOUCH0", str_detect(model_index, "PRSCHA_1|USNGSCH")) %>%
  select(model_index, term, estimate, std.error, p.value) %>% arrange(model_index)



# ----------
using_voucher_results %>% ggplot(aes(y = estimate, x = model_index)) +
  geom_point() + geom_errorbar(aes(ymax = estimate + std.error*1.96, ymin = estimate - std.error*1.96, width = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm"))


# -->
# PRSCHA_1:  around 6% effect both for base and base + covariate model
# For voucher-won group, the percentage of student who start 6th year in private school is increased around 6%


# USNGSCH:  around 50% effect both for base and base + covariate model
# For voucher-won group, the percentage of student who have used some scholoarship is 50% larger than
# voucher-lose group



# ------------------------------------------------------------------------------
# 留年の傾向を可視化
# effect of VOUCH0 for PRSCH_C, INSCHL, FINISH6-8, REPT
# ------------------------------------------------------------------------------

going_private_results <- df_results %>%
  filter(term == "VOUCH0", str_detect(model_index, "PRSCH_C|INSCHL|FINISH|REPT")) %>%
  select(model_index, term, estimate, std.error, p.value) %>%
  arrange(model_index)



# ----------
going_private_results %>% filter(str_detect(model_index, "covariate")) %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm"))



# -->
# PRSCH_C, INSCHL:  For voucher-won group, the number of student who continue schooling is NOT increased,
# but the number of student who continue schooling in private school IS increased

# REPT6:  for voucher-won group, the percentage of the student repeating same year at 6th grade is decreased around 6%
# REPT:  for voucher-won group, the percentage of the student who have repeated same year once or more is decreased
# NREPT:  for voucher-won group, number of times of repeating years is decreased

# FINISH6,7,8:  for voucher-won group, the percentage of the student who have finished 6th grade, 7th grade, and 8th grade is incrased.



