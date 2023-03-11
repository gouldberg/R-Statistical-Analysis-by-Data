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
# Add trend effect by variable year
# and apply inverse of Exchange Rate (How much dollars are purchaged by one Yen)
#   - Note that, in the model of inverse of Exchange Rate, 
#     the marginal effect of Exchange Rate is not constant since d CPI / d EXR = 1 / EXP^2 (not constant)
# ------------------------------------------------------------------------------

dat <- dat %>% mutate(exr_inv = 1 / exr)



# ----------
# no trend variable
mod_lin <- lm(cpi ~ exr + wage, data = dat)

mod_loglog <- lm(log(cpi) ~ log(exr) + log(wage), data = dat)

mod_lin_exrinv <- lm(cpi ~ exr_inv + wage, data = dat)




# ----------
# with trend variable
mod_lin_trend <- lm(cpi ~ exr + wage + year, data = dat)

mod_loglog_trend <- lm(log(cpi) ~ log(exr) + log(wage) + year, data = dat)

mod_lin_exrinv_trend <- lm(cpi ~ exr_inv + wage + year, data = dat)




# ----------
# Trend variable is significant but the coefficient value is almost same with wage
# Exchange Rate is not significant in the model with trend variable
summary(mod_lin)
summary(mod_lin_trend)



# ----------
# Including inverse of Exchange Rate, all coefficients are highly significant and Adjusted R^2 is larger than mod_lin
# Note that the coefficients of exr_inv is negative.
summary(mod_lin)
summary(mod_lin_exrinv)



# ----------
# Trend variable is not significant in loglog model
summary(mod_loglog)
summary(mod_loglog_trend)



# ----------
summary(mod_lin_exrinv)
summary(mod_lin_exrinv_trend)





