setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)



# ----------
# set the lowest category to zero in order to make it eRm compaatible
itceaq <- CEAQ[,1:16] - 1



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho




# ------------------------------------------------------------------------------
# Check model fit by Andersen's LR-test
# ------------------------------------------------------------------------------


# wer use grade as splitting criterion.
# Now that we have some missing grade values which we impute using the mice package, since we need to have full responses on the split criterion

library(mice)

set.seed(222)


imp <- mice(CEAQ)

gradevec <- complete(imp)$grade



# ----------
# binarize
levels(gradevec) <- c("grade56", "grade56", "grade78", "grade78")


LRtest(fitrsm2, gradevec)



# ----------
# p-value > 0.05, we can assume that the data fit the RSM



# ------------------------------------------------------------------------------
# Waldtest
# ------------------------------------------------------------------------------


Waldtest(fitrsm2, gradevec)



