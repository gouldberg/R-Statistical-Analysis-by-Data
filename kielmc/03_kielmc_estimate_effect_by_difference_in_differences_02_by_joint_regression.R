setwd("//media//kswada//MyFiles//R//kielmc")

packages <- c("dplyr", "foreign")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  kielmc
#   - Wooldridge (2016, Section 13.2) example:  effect of a garbage incinerator's location or housing prices
# ------------------------------------------------------------------------------

# library(foreign)
# kielmc <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/kielmc.dta")
# readr::write_tsv(kielmc, path = "kielmc.txt")

kielmc <- readr::read_tsv("kielmc.txt")


dim(kielmc)

str(kielmc)


car::some(kielmc)



# ------------------------------------------------------------------------------
# Estimate difference by joint regression
# ------------------------------------------------------------------------------

library(lmtest)


# Joint regression including an interaction term
# coeftest:  generic function for performing z and (quasi) t Wald tests of estimated coefficients.

coeftest(lm(rprice ~ nearinc * y81, data = kielmc))



# -->
# The diffenrece-in-differences is 11864 dollars, as same as the separate regressions
# the estimator 11864 can be directly seen as the coefficient of the interaction term.
# Conveniently, standard regression tables include t-tests of the hypothesis that the actual effect is equal to zero.
# For a one-sided test, p-value is 1/2 * 0.112 = 0.056, so there is some statistical evidence of a negative impact.



# ------------------------------------------------------------------------------
# Estimate difference by joint regression, in log
# ------------------------------------------------------------------------------

# a logarithmic specification is more plausible since it implies a constant percentage effect on the house values

coeftest(lm(lrprice ~ nearinc * y81, data = kielmc))



# -->
# but here, the interaction term is not statistically significant.


