# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

library(systemfit)



# ------------------------------------------------------------------------------
# data:  mroz  (from wooldridge !!!)
# ------------------------------------------------------------------------------
mroz <- read.dta("http://fmwww.bc.edu//ec-p//data//wooldridge//mroz.dta")

str(mroz)

names(mroz)

oursample <- subset(mroz, !is.na(wage))



# ------------------------------------------------------------------------------
# Estimate labor supply of married, working women by 3SLS
#   - 3SLS estimator adds another stage to 2SLS by estimating the correlation and accounting for it using a FGLS approach.
# ------------------------------------------------------------------------------

# Define system of equations and instruments

eq.hrs <- hours ~ log(wage) + educ + age + kidslt6 + nwifeinc

eq.wage <- log(wage) ~ hours + educ + exper + I(exper^2)

eq.system <- list(eq.hrs, eq.wage)

instrum <- ~ educ + age + kidslt6 + nwifeinc + exper + I(exper^2)



# ----------
# 3SLS of whole system

mroz.sysfit_3SLS <- systemfit(eq.system, inst = instrum, data = oursample, method = "3SLS")

summary(mroz.sysfit_3SLS)

summary(mroz.sysfit)



# -->
# The coefficients of hours is slightly different

# Correlation between the residuals of the equations is -0.94, more negative than the estimation by 2SLS (= -0.90)



