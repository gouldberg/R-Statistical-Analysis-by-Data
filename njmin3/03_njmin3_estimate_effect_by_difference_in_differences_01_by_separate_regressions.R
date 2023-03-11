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
# Estimate difference by separate regressions
# ------------------------------------------------------------------------------

library(broom)


# Separate regressions for before and after the change of minimum wage law in New Jersey:  report coefficients only

njmin3 %>% lm(data = ., fte ~ as.factor(nj), subset = (d == 0)) %>% tidy()

njmin3 %>% lm(data = ., fte ~ as.factor(nj), subset = (d == 1)) %>% tidy()



# -->
# after the change of minimum wage law, fte in New Jersey was smaller by 2.89 in average
# but this was not only due to the law change, since even before, New Jersey was smaller by 0.138 in average

# The difference of these differences = 2.89 - 0.138 = 2.75 is the DiD (difference-in-differences) estimator
# and is arguably a better indicator of the actual effect.

# Note that change of minimum wage law has positive effect !!!  (by 2.75)
# although actual number of fte is not increased much, almost stay.


