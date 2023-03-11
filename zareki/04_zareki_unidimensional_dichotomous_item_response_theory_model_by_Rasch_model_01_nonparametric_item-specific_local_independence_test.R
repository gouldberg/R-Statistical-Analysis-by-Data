setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# We consider eight binary subtraction items only
zarsub <- zareki[, grep("subtr", colnames(zareki))]

str(zarsub)



# ----------
# compute tetrachoric correlation matrix

library(psych)

tetcor <- tetrachoric(zarsub)

tetcor

tetcor$rho



# ------------------------------------------------------------------------------
# Nonparametric test for test assumptions more explicitly
# ------------------------------------------------------------------------------

set.seed(123)



# ----------
# We examine item-specific local independence (T1-test)
T1_1 <- NPtest(as.matrix(zarsub), n = 1000, method = "T1")

T1_2 <- NPtest(as.matrix(zarsub[,-5]), n = 1000, method = "T1")


T1_1

T1_2



# ----------
# We examine item-specific local independence at a global level (T11-test)
T11_1 <- NPtest(as.matrix(zarsub), n = 1000, method = "T11")

T11_2 <- NPtest(as.matrix(zarsub[,-5]), n = 1000, method = "T11")


T11_1

T11_2



# -->
# The first output suggests that none of the item pairs show significant local dependence.
# The second test output confirms this result by telling us that local independence holds at a global level (but suspicious for T11_1)

