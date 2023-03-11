setwd("//media//kswada//MyFiles//R//njmin3")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  njmin3
# ------------------------------------------------------------------------------

data("njmin3", package = "POE5Rdata")

dim(njmin3)

str(njmin3)


car::some(njmin3)



# ------------------------------------------------------------------------------
# Estimate difference by joint regression
# ------------------------------------------------------------------------------

library(lmtest)


# Joint regression including an interaction term
# coeftest:  generic function for performing z and (quasi) t Wald tests of estimated coefficients.

coeftest(lm(fte ~ nj * d, data = njmin3))



# -->
# The diffenrece-in-differences is 2.75, as same as the separate regressions
# the estimator 2.75 can be directly seen as the coefficient of the interaction term.
# Conveniently, standard regression tables include t-tests of the hypothesis that the actual effect is equal to zero.
# For a one-sided test, p-value is 1/2 * 0.10331 = 0.0515, so there is some statistical evidence of a negative impact.



# ------------------------------------------------------------------------------
# Estimate difference by joint regression, in log
# ------------------------------------------------------------------------------

# a logarithmic specification is more plausible since it implies a constant percentage effect on fte


# we need to adjust by adding small value since some fte is zero, not feasible for log
tmp <- njmin3 %>% mutate(fte2 = fte + 0.001)

coeftest(lm(log(fte2) ~ nj * d, data = tmp))


# -->
# but here, the interaction term is not statistically significant.


