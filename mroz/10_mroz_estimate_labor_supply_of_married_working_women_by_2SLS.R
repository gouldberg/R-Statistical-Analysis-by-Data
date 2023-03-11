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
# Estimate labor supply of married, working women by 2SLS (ivreg)
#   - Estimation of each equation separately by 2SLS is straightforward once we have set up the system and ensured identification.
#     The excluded regressors in each equation serve as instrumental variables
# ------------------------------------------------------------------------------
# Note that same variables are applied as IV in two equations

mroz1.iv1 <- ivreg(hours ~ log(wage) + educ + age + kidslt6 + nwifeinc | educ + age + kidslt6 + nwifeinc + exper + I(exper^2), data = oursample)

summary(mroz1.iv1, diagnostics = TRUE)



# ----------
mroz1.iv2 <- ivreg(log(wage) ~ hours + educ + exper + I(exper^2) | educ + age + kidslt6 + nwifeinc + exper + I(exper^2), data = oursample)

summary(mroz1.iv2, diagnostics = TRUE)



# ------------------------------------------------------------------------------
# Estimate labor supply of married, working women by joint estimation of system (systemfit)
# ------------------------------------------------------------------------------

# Define system of equations and instruments

eq.hrs <- hours ~ log(wage) + educ + age + kidslt6 + nwifeinc

eq.wage <- log(wage) ~ hours + educ + exper + I(exper^2)

eq.system <- list(eq.hrs, eq.wage)

instrum <- ~ educ + age + kidslt6 + nwifeinc + exper + I(exper^2)



# ----------
# 2SLS of whole system

mroz.sysfit <- systemfit(eq.system, inst = instrum, data = oursample, method = "2SLS")

summary(mroz.sysfit)

summary(mroz1.iv2)



# -->
# note that two results are same (both are estimated by 2SLS)

# The results of systemfit provides additional information, the correlation between the residuals of the equations.
# It is reported to be a substantially negative -0.90.
# We can account for the correlation between the error terms to derive a potentially more efficient parameter estimation than 2SLS. --> 3SLS

