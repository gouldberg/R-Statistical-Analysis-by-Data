setwd("//media//kswada//MyFiles//R//crabs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crabs
# ------------------------------------------------------------------------------

data("crabs", package = "MASS")


str(crabs)


car::some(crabs)



# ------------------------------------------------------------------------------
# Test for isometry
#   - The tests for isometry are based on the sample correlation between residuals and fitted values.
# ------------------------------------------------------------------------------

# slope.test():  One-sample test of a (standardised) major axis slope
# The test assumes:
#  1. y and x are linearly related
#  2. residuals independently follow a normal distribution with equal variance at all points along the line

test_f <- slope.test(log(FFL), log(FRW), method = "MA")
unlist(test_f)


test_m <- slope.test(log(MFL), log(MRW), method = "MA")
unlist(test_m)


# -->
# r: the test statistic - the sample correlation between residuals and fitted values
# p: the p-value, taken from the F-distribution, this is the exact test if residuals are normally distributed
# test.value:  the hypothesized value of the slope
# b: the estimated slope
# ci: confidence interval for the slope

# While females have a growth pattern not different from isometry,
# males show a significant allometric growth pattern.


# ----------
# for reference: apply OLS
unlist(slope.test(log(FFL), log(FRW), method = "OLS"))

unlist(slope.test(log(MFL), log(MRW), method = "OLS"))



# ------------------------------------------------------------------------------
# Test whether there is a difference in allometric growth between sexes
# ------------------------------------------------------------------------------

# slope.com():  Common slope test amongst several allometric lines
unlist(slope.com(crabs$FL[1:100], crabs$RW[1:100], groups = crabs$sex[1:100], method = "MA"))


# -->
# The test for common slopes shows that growth gradients for both measurements are different between males and females

