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
# simple wage equation
# ------------------------------------------------------------------------------

# Consider the simple wage equation  --> coeff of educ:  0.1086
w0 <- lm(log(wage) ~ educ, data = mroz_lfp1)

kable(tidy(w0), digits = 4, align = "c", caption = "OLS Estimation of a Simple $Wage$ Regression")



# ----------
# OLS slope parameter manually
with(mroz_lfp1, cov(log(wage), educ) / var(educ))



# ------------------------------------------------------------------------------
# OLS regression
# ------------------------------------------------------------------------------

# regression for Mincer equation
w1 <- lm(log(wage) ~ educ + exper + I(exper^2), data = mroz_lfp1)

summary(w1)


kable(tidy(w1), digits = 4, align = "c", caption = "OLS Estimation of the $Wage$ Equation")


# -->
# educ coefficient:  0.1075
# one extra year of education increases the average wage for the women in the labor force by 10.75%
# notorious difficulty with this model is that the error term may include some unobserved attrributes, such as personal ability,
# that determines both wage and education.
# Thus the independent variable educ is correlated with the error term or, in other words, it is endonegenous


# ----------
plot(w1)



# ------------------------------------------------------------------------------
# simple wage equation
# ------------------------------------------------------------------------------

# Consider the simple wage equation  --> coeff of educ:  0.1086
w0 <- lm(log(wage) ~ educ, data = mroz_lfp1)

kable(tidy(w0), digits = 4, align = "c", caption = "OLS Estimation of a Simple $Wage$ Regression")



# ----------
# OLS slope parameter manually
with(mroz_lfp1, cov(log(wage), educ) / var(educ))


