# setwd("//media//kswada//MyFiles//R//european_valuesstudy")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//european_valuesstudy")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  European Values Study
# ------------------------------------------------------------------------------

data("EuropeanValuesStudy", package = "psychotree")


str(EuropeanValuesStudy)


car::some(EuropeanValuesStudy)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

library(psychotools)



EuropeanValuesStudy$paircomp



# preference is "paircomp" class,  some are NA's
summary(EuropeanValuesStudy$paircomp)



# covariates(EuropeanValuesStudy$paircomp)

