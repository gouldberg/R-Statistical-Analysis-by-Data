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
# Weak instruments test
#   - To test for weak instruments in the wage equation, we just test the joint significance of the instruments
#     in an educ model
# ------------------------------------------------------------------------------

educ.ols <- lm(educ ~ exper + I(exper^2) + mothereduc + fathereduc, data = mroz_lfp1)


summary(educ.ols)


kable(tidy(educ.ols), digits = 4, caption = "The $Educ$ First-Stage Equation")



# -->
# Indicating a strong dependency of the variable educ on both mothereduc and fathereduc.



# ----------
# requires testing both coefficients simultaneously
car::linearHypothesis(educ.ols, c("mothereduc=0", "fathereduc=0"))



# -->
# The test rejects the null hypothesis that both mothereduc and fathereduc coefficients are zero,
# indicating that at least one instrument is strong.

# RULE OF THUMB:  soundly reject the H0 at a value of the F stats greater than 10 or, for only one instrument, a t-stats greater than 3.16
# to make sure that an instrument is strong



# ------------------------------------------------------------------------------
# Test for exogeneity of the regressors
#   - The second stage adds the first stage residuals to the origianl list of regressors.
#     We can directly interpret the t test from the regression table as a test for exogeneity.
# ------------------------------------------------------------------------------

stage2 <- lm(log(wage) ~ educ + exper + I(exper^2) + resid(educ.ols), data = mroz_lfp1)


coeftest(stage2)


# -->
# for resid(educ.ols), t = 1.6711 with a two-sided p-value of p = 0.095, indicating a marginally significant evidence for endogeneity.

