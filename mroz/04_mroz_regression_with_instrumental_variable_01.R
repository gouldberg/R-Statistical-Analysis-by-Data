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
# Instrutamntal variable: mothereduc
#   - An instrument that may address the endogeneity of educ is mothereduc, of which we can reasonably assume that it does not directly influence
#     the daughter's wage (it is contemporaneously correlated with the independent variable), but it influences the daughter's education.
# ------------------------------------------------------------------------------

w0iv <- AER::ivreg(log(wage) ~ educ | mothereduc, data = mroz_lfp1)


kable(tidy(w0iv), digits = 4, align = "c", caption = "IV Estimation of a Simple $Wage$ Regression")


# -->
# IV version:  educ is not significant
# sugessting that the efect of education on the expected wage is 3.8% , about smaller than our previous estimates.
# And the educ is NOT significant.



# ----------
# IV slope parameter manually
with(mroz_lfp1, cov(log(wage), fathereduc) / cov(educ, fathereduc))




# ------------------------------------------------------------------------------
# Instrutamntal variable: fathereduc + mothereduc
# ------------------------------------------------------------------------------

w0iv2 <- AER::ivreg(log(wage) ~ educ | mothereduc + fathereduc, data = mroz_lfp1)


kable(tidy(w0iv2), digits = 4, align = "c", caption = "IV Estimation of a Simple $Wage$ Regression")
