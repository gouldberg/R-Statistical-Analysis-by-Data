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



evs <- na.omit(EuropeanValuesStudy)



# ------------------------------------------------------------------------------
# Bradley Terry Trees:  test parameter instability at each node
# ------------------------------------------------------------------------------

library("strucchange")


sctest(bt, node = 1)


sctest(bt, node = 4)


sctest(bt, node = 10)


sctest(bt, node = 9)


sctest(bt, node = 13)



# -->
# In node 9, still "employment" has low p.value  (< 0.05)
# In node 13, still "gender" has low p.value  (< 0.05)

