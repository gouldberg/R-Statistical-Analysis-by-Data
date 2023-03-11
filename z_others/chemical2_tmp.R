# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "plm")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  chemical2
#
# Estimate chemical production function
#
# First Difference Model (FD):  assuming only two time periods, t = 1, 2
#  - origianl:  y(it) = beta1 + beta2 * x2(it) + alpha1 * w1(t) + u(i) + e1
#  - taking differences between two time periods:  delta y(i) = beta2 * delta x(i2) + delta ei
#      --> endogenous individual attributs have been dropped, but the parameter of interest (beta2) has been preserved  --> beta2 is called "difference estimator"
# ------------------------------------------------------------------------------
data("chemical2", package = "POE5Rdata")
data_o <- chemical2
dim(data_o)
str(data_o)
describe(data_o)

car::some(data_o)


# restrict data to years 2005-2006
data <- subset(data_o, year %in% c(2005,2006))
car::some(data)


# OLS estimation
chem.ols <- lm(lsales ~ lcapital + llabor, data = data)
kable(tidy(chem.ols), digits = 4, align="c", caption = "OLS Estimate of the chemical Production Function")


# Define panel data
data.p <- pdata.frame(data, index = c("firm", "year"))
pdim(data.p)

chem.plm <- plm(lsales ~ lcapital + llabor - 1, data = data.p, model = "fd")
kable(tidy(chem.plm), digits = 4, align="c", caption = "Difference Estimator of the chemical Production Function")



# ------------------------------------------------------------------------------
# data:  chemical2
#
# Within Estimator:  the difference is between a variable and its mean over all time periods
#  - allowing more than two time periods
# ------------------------------------------------------------------------------
# data with sales data for years 2004 - 2006
data <- pdata.frame(data_o)


# within model
chem.wthn <- plm(lsales ~ lcapital + llabor, data = data, model = "within", effect = "time")
tidy(chem.wthn)


# ------------------------------------------------------------------------------
# data:  chemical2
#
# Least Squares Dummy Variable Model (Fixed Effects Model)
#  - A dummy variable (or fixed effects) model includs a dummy variable for each individual in the panel
#  - This model is equivalent to the "within" model above.
#  - The fixed effects model requires testing for unobserved heterogeneity or individual differences,
#     H0: all coeffs of the dummy variables are zero
# ------------------------------------------------------------------------------
# within model
chem.wthn <- plm(lsales ~ lcapital + llabor, data = data, model = "within", effect = "time")


# ols (pooled model)
chem.ols <-  lm(lsales ~ lcapital + llabor, data = data)


# H0 of no fixed effects is rejected
tst <- pFtest(chem.wthn, chem.ols)
kable(tidy(tst), caption = "Fixed Effects Test: H0: No Individual Heterogeneity")



# ------------------------------------------------------------------------------
# data:  chemical3
#







