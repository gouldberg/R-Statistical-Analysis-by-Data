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
# Inference:  Test of H0: experience and age are irrelevant
# ------------------------------------------------------------------------------

restr <- glm(inlf ~ nwifeinc + educ + kidslt6 + kidsge6, family = binomial(link = probit), data = mroz)

lrtest(restr, probitres)



# ------------------------------------------------------------------------------
# predictions for two "extreme" women
# ------------------------------------------------------------------------------
# Women 1:  20 years old, has no work experience, 5 years of education, two children below age 6 and has additional family income of 100,000 USD
# Women 2:  52 years old, has 30 years of work experience, 17 years of education, no children and no other source of income
xpred <- list(nwifeinc = c(100, 0), educ = c(5, 17), exper = c(0, 30), age = c(20, 52), kidslt6 = c(2, 0), kidsge6 = c(0, 0))

predict(linprob, xpred)
predict(logitres, xpred, type = "response")
predict(probitres, xpred, type = "response")

