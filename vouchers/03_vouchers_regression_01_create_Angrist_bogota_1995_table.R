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
# Angrist (2002) Table 3. bogota 1995
# preparation
# ------------------------------------------------------------------------------

# "VOUCH0"
formula_x_base <- "VOUCH0"

formula_x_covariate <- "SVY + HSVISIT + AGE + STRATA1 + STRATA2 + STRATA3 + STRATA4 + STRATA5 + STRATA6 + STRATAMS + D1993 + D1995 + D1997 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11 + DMONTH12 + SEX2"

formula_y <- c("TOTSCYRS","INSCHL","PRSCH_C","USNGSCH","PRSCHA_1","FINISH6","FINISH7","FINISH8","REPT6","REPT","NREPT",
               "MARRIED","HASCHILD","HOURSUM","WORKING3")


# ----------
base_reg_formula <- paste(formula_y, "~", formula_x_base)

names(base_reg_formula) <- paste(formula_y, "base", sep = "_")

base_reg_formula



# ----------
covariate_reg_formula <- paste(formula_y, "~", formula_x_base, "+", formula_x_covariate)

names(covariate_reg_formula) <- paste(formula_y, "covariate", sep = "_")

table3_fomula <- c(base_reg_formula, covariate_reg_formula)

models <- table3_fomula %>% enframe(name = "model_index", value = "formula")


models


print(tbl_df(models), n = 30)



# ------------------------------------------------------------------------------
# Angrist (2002) Table 3. bogota 1995
# Regression
# ------------------------------------------------------------------------------

# filter TAB3SMPL == 1 AND BOG955SMP == 1
regression_data <- vouchers %>% filter(TAB3SMPL == 1, BOG95SMP == 1)


df_models <- models %>%
  mutate(model = map(.x = formula,
                     .f = lm,
                     data = regression_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))



# ----------
df_results <- df_models %>%
  mutate(formula = as.character(formula)) %>%
  select(formula, model_index, lm_result) %>%
  unnest(cols = c(lm_result))


df_results




# ----------
# Note that this model DOES NOT CARE ABOUT model terms's significance !!!

regression_data <- vouchers %>% filter(TAB3SMPL == 1, BOG95SMP == 1)

tmp_reg <- lm(PRSCH_C ~ VOUCH0 + SVY + HSVISIT + AGE + STRATA1 + STRATA2 + STRATA3 + STRATA4 + STRATA5 + STRATA6 + STRATAMS + D1993 + D1995 + D1997 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11 + DMONTH12 + SEX2, data = regression_data)

summary(tmp_reg)

