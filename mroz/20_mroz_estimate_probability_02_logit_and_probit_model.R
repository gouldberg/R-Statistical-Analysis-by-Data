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
# Estimate the probability that a woman is in the labor force:  logit model and probit model
# ------------------------------------------------------------------------------

logitres <- glm(inlf ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, family = binomial(link = logit), data = mroz)

probitres <- glm(inlf ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, family = binomial(link = probit), data = mroz)


summary(logitres)

summary(probitres)



# ----------
# Log likelihood value
logLik(logitres)
logLik(probitres)



# ----------
# McFadden's pseudo R2
1 - logitres$deviance / logitres$null.deviance
1 - probitres$deviance / logitres$null.deviance


# -->
# probit model is slightly better.



