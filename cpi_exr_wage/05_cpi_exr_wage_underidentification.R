# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//cpi_exr_wage")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CPI, EXR and WAGE
# ------------------------------------------------------------------------------

dat <- read.csv("cpi_exr_wage_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


Hmisc::describe(dat)



# ------------------------------------------------------------------------------
# Check the difference of coefficient value of Exchange Rate in underidentification model and that in original model
# ------------------------------------------------------------------------------

# original log-log model
mod_loglog <- lm(log(cpi) ~ log(exr) + log(wage), data = dat)

b_exr <- coef(mod_loglog)[2]
b_wage <- coef(mod_loglog)[3]



# ----------
# Underidentification:  lacks log(wage)
mod_loglog_under <- lm(log(cpi) ~ log(exr), data = dat)

b_exr_under <- coef(mod_loglog_under)[2]



# ----------
# log(Wage) ~ log(exr)
mod_loglog_wage <- lm(log(wage) ~ log(exr), data = dat)

b_wage_exr <- coef(mod_loglog_wage)[2]



# ----------
# Check underidentification
# The coefficient for Exchange Ratio in underidentification model is larger than that for original model
# by d Wage / d Exr * b_wage  (effect of wage through Exchange Rage)

b_exr_under - b_exr

b_wage_exr * b_wage


