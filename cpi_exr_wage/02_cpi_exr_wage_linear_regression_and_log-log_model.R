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
# linear model
# ------------------------------------------------------------------------------

mod_lin <- lm(cpi ~ exr + wage, data = dat)


summary(mod_lin)


cor(dat$exr, dat$cpi)



# -->
# correlation among Exchange Rate and CPI is negative,
# but coefficients of exchange rate in thie multiple regression is positive value = 0.053
# If the wage is controlled, net effect of Exchange Rate is postive to CPI !!!




# ------------------------------------------------------------------------------
# log-log model
# ------------------------------------------------------------------------------

mod_loglog <- lm(log(cpi) ~ log(exr) + log(wage), data = dat)


summary(mod_loglog)



# -->
# Elasticity of Exchange Rate is 0.0534
# Elasticity of Wage is 0.0634
# Intercept is also significant



# ------------------------------------------------------------------------------
# Check residuals
# ------------------------------------------------------------------------------

dat$resid_lin <- resid(mod_lin)
dat$resid_loglog <- resid(mod_loglog)



# ----------
par(mfrow = c(2,4))
plot(dat$year, dat$resid_lin)
plot(dat$exr, dat$resid_lin)
plot(dat$wage, dat$resid_lin)
plot(dat$cpi, dat$resid_lin)

plot(dat$year, dat$resid_loglog)
plot(dat$exr, dat$resid_loglog)
plot(dat$wage, dat$resid_loglog)
plot(dat$cpi, dat$resid_loglog)



# -->
# in linear model, residuals are largest at year = 1980
# in log-log model, residuals are largest at year = 1970


