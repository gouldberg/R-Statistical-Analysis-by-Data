setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mroz
# ------------------------------------------------------------------------------

library(foreign)


# This data has "inlf" variable
mroz <- read.dta("http://fmwww.bc.edu//ec-p//data//wooldridge//mroz.dta")

str(mroz)

names(mroz)


# oursample <- subset(mroz, !is.na(wage))



# ------------------------------------------------------------------------------
# Estimate the probability that a woman is in the labor force:  sample selection corrections (Heckit model)
#   - Heckman's selection model consists of a probit-like model for the binary fact whether y is observed and a linear regression-like model for y.
#     Selection can be driven by the same determinants as y but should have at least one additional factor excluded from the equation for y.
# ------------------------------------------------------------------------------

# Of the 753 women, 428 worked (inlf=1) and the rest did not work (inlf=0).
# For the latter, we do not observe the wage they would have gotten had they worked.
table(mroz$inlf)



# ----------
# Estimate Heckman selection model (2 step version)
# It expects two formulas:  One for the selection and one for the wage equation
# The option method = "step" requests implicit 2-step estimation to make the results comparable to those reported by Wooldridge (2016).

library(sampleSelection)

res <- selection(inlf ~ educ + exper + I(exper^2) + nwifeinc + age + kidslt6 + kidsge6,
                 log(wage) ~ educ + exper + I(exper^2), data = mroz, method = "2step")


summary(res)



# ----------
# more efficient MLE
res_ML <- selection(inlf ~ educ + exper + I(exper^2) + nwifeinc + age + kidslt6 + kidsge6,
                    log(wage) ~ educ + exper + I(exper^2), data = mroz, method = "ml")


summary(res_ML)
