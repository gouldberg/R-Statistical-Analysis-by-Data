setwd("//media//kswada//MyFiles//R//jobsat")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  JobSat
#  - data on job satisfaction classified by income and level of satisfaction from a 4 * 4 table given by Agresti (2002)
# ------------------------------------------------------------------------------
data <- matrix(c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11), nrow = 4, ncol = 4)

dimnames(data) <- list(income = c("< 15k", "15-25k", "25-40k", "> 40k"), satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS"))

data <- as.table(data)

data



# ------------------------------------------------------------------------------
# Sieve diagram
# ------------------------------------------------------------------------------
sieve(data, shade=TRUE)



# ------------------------------------------------------------------------------
# Cramer'sV is small =  0.144
# ------------------------------------------------------------------------------
assocstats(data)



# ------------------------------------------------------------------------------
# Test of association by chisq.test()
# ------------------------------------------------------------------------------
# This test may not be appropriate because there are many cells with value less than 5.
chisq.test(data)



# ------------------------------------------------------------------------------
# Test of association by chisq.test() with Monte Carlo test that does not depend on lage sample size
# ------------------------------------------------------------------------------
# The result does not change much even with Monte Corlo simulation test
chisq.test(data, simulate.p.value = TRUE)



# ------------------------------------------------------------------------------
# CMHtest:  Both variables are ordianl, so CMH tests may be more powerful here.
# ------------------------------------------------------------------------------
CMHtest(data)


# --> even with CMH testm, not rejecting Nonzero correlation (p = 0.08)


