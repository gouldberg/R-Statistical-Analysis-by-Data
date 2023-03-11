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
# Add dummy variable as year == 1980, when the residuals are largest
# ------------------------------------------------------------------------------

dat <- dat %>% mutate(yr_flg = ifelse(year == 1980, 1, 0))


mod_lin2 <- lm(cpi ~ exr + wage + yr_flg, data = dat)




# ----------
# But the models are not big different, since year == 1980 is only one data and residuals for this data is ZERO
summary(mod_lin)
summary(mod_lin2)


anova(mod_lin, mod_lin2)



# ------------------------------------------------------------------------------
# Remove year == 1980, when the residuals are largest
# ------------------------------------------------------------------------------

mod_lin3 <- lm(cpi ~ exr + wage, data = subset(dat, yr_flg == 0))




# ----------
# But the models are not big different
summary(mod_lin2)
summary(mod_lin3)



# -->
# We cannot detect structural change in this analysis



