setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------

data("psid", package = "faraway")


str(psid)

dim(psid)


car::some(psid)



# ------------------------------------------------------------------------------
# Mixed effects model considering year-to-year variation within each subject
# ------------------------------------------------------------------------------

# We consider other factors that will affect a subject's income.
# We also expect that there will be some year-to-year variation within each subject.
# For simplicity, initially assume that this error is homogeneous and uncorrelated.

library(lme4)


psid$cyear <- psid$year - 78


mmod <- lmer(log(income) ~ cyear * sex + age + educ + (cyear | person), data = psid)



# ----------
summary(mmod)



# -->
# Fixed effects:
# Income increase about 10% for each additional year of education.
# We see that age does not appear to be sifnificant.
# For females, the reference level in this example, income increases about 8.5% a year, while for men, it increases about 8.5 - 2.6 = 5.9% a year.
# We see that the income of men are exp(1.15) = 3.16 times higher.


# Random effects:
# The standard deviation for the intercept and slope are 0.531 and 0.049 respectively.
# These have a correlation of 0.189.
# Finally, there is one additional variation in the measurement not so far accounted for having standard deviation of 0.684.
# We see that the variation in increase in income is relatively small while the variation in overall income between individuals is quite large.
# Given the large residual variation, there is a large year-to-year variation in incomes.

