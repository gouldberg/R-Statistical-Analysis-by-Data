# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "plm")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  wagepan
#
# balanced panel for n = 545 individuals over T = 8 years
# 
# Estimate the change of the return to education over time using a fixed effects estimator (FE)
# ------------------------------------------------------------------------------
data <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta")
dim(data)
str(data)
describe(data)

car::some(data)


# define panel data frame
data.p <- pdata.frame(data, index = c("nr", "year"))
pdim(data.p)
str(data.p)

car::some(data.p)


# estimate fixed effects (FE) model:  demeaning within "nr"
# Since educ does not change over time, we cannot estimate its overall impact. However we can interact it with time dummies to see how the impact changes over time
# Using plm package has the advantage that the defress of freedom are adjusted to the demeaning and the variance-covariance matrix and
# standard erros are adjusted accordingly
mod <- plm(lwage ~ married + union + factor(year) * educ, data = data.p, model = "within")
summary(mod)


# ------------------------------------------------------------------------------
# data:  wagepan
#
# Random Effects Models (RF)
# Hausman test
# ------------------------------------------------------------------------------
# check variation of variables within individuals
# get a list of time-constant variables
pvar(data.p)

data.p$yr <- factor(data.p$year)


# estimate different models
reg.ols <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union + yr, data = data.p, model = "pooling")  # pooled OLS
reg.re <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union + yr, data = data.p, model = "random")
reg.fe <- plm(lwage ~ educ + black + hisp + exper + I(exper^2) + married + union + yr, data = data.p, model = "within")


# not reporting year dummies
stargazer(reg.ols, reg.re, reg.fe, type = "text",
          column.labels = c("OLS", "RE", "FE"),
          keep.stat = c("n", "rsq"),
          keep = c("ed", "bl", "hi", "exp", "mar", "un"))


# Housman test:
# This test is based on the comparison between the FE and RE parameter estimates
# The null hypothesis that the RE model is consistent is clearly rejected --> 
phtest(reg.fe, reg.re)


# ------------------------------------------------------------------------------
# data:  wagepan
#
# Dummy variable regression and correlated random effects (CRE)
#
# CRE:  a(i) = gamma(0) + gamma1 * x-bar(i1) + gamma2 * x-bar(i2) + ... + gamma(k) * x-bar(ik) + r(i)
#  if r(i) is uncorrelated with the regressors, we can consistently estimate the parameters of this model using the RE estimator
#  In addition to the original regressors, we include their averages over time
#  a(i) includes intelligence and other labor market success factors
# ------------------------------------------------------------------------------
# estimate FE parameter in 3 different ways
reg.fe <- plm(lwage ~ married + union + yr * educ, data = data.p, model = "within")
reg.dum <- lm(lwage ~ married + union + yr * educ + factor(nr), data = data.p)
reg.re <- plm(lwage ~ married + union + yr * educ, data = data.p, model = "random")

# Correlated Random Effects (CRE)
reg.cre <- plm(lwage ~ married + union + yr * educ + Between(married) + Between(union), data = data.p, model = "random")


# The results confirm that the first three methods deliver exactly the same parameter estimates,
# while the RE estimates differ.
stargazer(reg.fe, reg.dum, reg.cre, reg.re, type = "text",
          model.names = FALSE,
          keep = c("married", "union", ":educ"),
          keep.stat = c("n", "rsq"),
          column.labels = c("Within", "Dummies", "CRE", "RE"))


# RE test as an F test on the "Between" coefficients
# Test H0 that gamma1 = gamma2 = ... = gammak = 0
# Clearly reject the H0 that the RE model is appropriate  --> CRE is better
car::linearHypothesis(reg.cre, matchCoefs(reg.cre, "Between"))


# Given a(i) includes intelligence and other labor market success factors, 
reg.cre2 <- plm(lwage ~ married + union + educ + black + hisp + Between(married) + Between(union), data = data.p, model = "random")
summary(reg.cre2)


# robust (clustered) standard errors
lmtest::coeftest(reg.cre, vcovHC)
lmtest::coeftest(reg.cre2, vcovHC)

