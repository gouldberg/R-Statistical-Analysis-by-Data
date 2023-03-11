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
# Student's t-test for slope difference
# ------------------------------------------------------------------------------

# regression parameters and statistics for the first group
a <- summary(lm(crabs$RW[1:50] ~ crabs$FL[1:50]))$coefficients

a



# ----------
# regression parameters and statistics for the second group
b <- summary(lm(crabs$RW[51:100] ~ crabs$FL[51:100]))$coefficients

b



# ----------
# Computation of the t-value
( tt <- ( a[2,1] - b[2,1] ) / sqrt(a[2,2]^2 + b[2^2]^2) )



# ----------
# Computation of p-value
( 1 - pt(abs(-tt), (length(crabs$FL[1:100]) - 4))) * 2



# ------------------------------------------------------------------------------
# Test for slope differences between sexes by anova()
# ------------------------------------------------------------------------------

# We will consider only the first species of crabs, and determine whether sex influences morphology and interacts with growth
mod1 <- lm(crabs$RW[1:100] ~ crabs$FL[1:100])

mod2 <- lm(crabs$RW[1:100] ~ crabs$FL[1:100] + crabs$sex[1:100])

fullmod <- lm(crabs$RW[1:100] ~ crabs$FL[1:100] * crabs$sex[1:100])



# ----------
anova(fullmod, mod1)


# -->
# the regression functions differ between sexes.



# ----------
anova(fullmod, mod2)


# -->
# The slopes are different between both groups.

