setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mroz
# ------------------------------------------------------------------------------

data(mroz, package = "POE5Rdata")


dim(mroz)

str(mroz)


# ----------
# restrict to wome in the labour force
mroz_lfp1 <- subset(mroz, lfp == 1)



# ------------------------------------------------------------------------------
# IV (instrumental variables) estimation with 2-Stage Least Squares
# First Stage to regress educ on the instrument
# 2nd stage to create the fitted values of educ from the 1st stage and plug them into the model of interest to replace the original variable educ
# ------------------------------------------------------------------------------

# First Stage to regress educ on the instrument
educ.ols <- lm(educ ~ mothereduc, data = mroz_lfp1)

kable(tidy(educ.ols), digits = 4, align = "c", caption = "1st Stage in the 2SLS model for $Wage$ Equation")


# -->
# very low pval indicates a strong correlation between the instrument mothereduc and the endogenous variable educ


# ----------
# 2nd stage to create the fitted values of educ from the 1st stage and plug them into the model of interest to replace the original variable educ
educHat <- fitted(educ.ols)

wage.2sls <- lm(log(wage) ~ educHat, data = mroz_lfp1)

kable(tidy(wage.2sls), digits = 4, align = "c", caption = "2nd Stage in the 2SLS model for $Wage$ Equation")


# -->
# NOTE that the standard errors calculated in this way are incorrect  --> pval is not correct



# ------------------------------------------------------------------------------
# IV (instrumental variables) estimation with 2-Stage Least Squares
# Using surplus instruments: fathereduc
#   - When more than one instrument is available for an endogenous variable, the first stage in the 2SLS method can use them for
#     obtaining a better estimate of the fitted values of the endogenous variable
# ------------------------------------------------------------------------------

educ.2sls1 <- lm(educ ~ mothereduc + fathereduc, data = mroz_lfp1)

educhat <- fitted(educ.2sls1)

wage.2sls2 <- lm(log(wage) ~ educhat, data = mroz_lfp1)

kable(tidy(wage.2sls2), digits = 4, align = "c", caption = "A 2SLS model for $Wage$ Equation")



# ------------------------------------------------------------------------------
# Compare OLS, IV with 2SLS, and IV with ivreg
# IV (instrumental variables) estimation in Multiple Regression
# ------------------------------------------------------------------------------

# ordinary least squares regression
mroz1.ols <- lm(log(wage) ~ educ + exper + I(exper^2), data = mroz_lfp1)



# ----------
# two-stage least squares regression by lm
educ.2SLS1 <- lm(educ ~ exper + I(exper^2) + fathereduc + mothereduc, data = mroz_lfp1)

educHat <- fitted(educ.2SLS1)

wage.2sls <- lm(log(wage) ~ educHat + exper + I(exper^2), data = mroz_lfp1)



# ----------
# ivreg
mroz1.iv1 <- AER::ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + mothereduc + fathereduc, data = mroz_lfp1)




# ----------
# comparison

summary(mroz1.ols)

summary(wage.2sls)

summary(mroz1.iv1)


# -->
# Note that standard errors by ivreg is slightly smaller than 2SLS by lm


# this produces error
# stargazer(mroz1.ols, wage.2sls, mroz1.iv1, type = "text")



# -->
# The imporance of education in determining wage decreases in the IV model.
# The explicit 2SLS model and the iV model yield the same coefficients
#   - the educ coeffs in the IV model is equivalent to the educHat coeffs in 2SLS,
# but the standard erros are different. The correct ones are those provided by the IV model.

