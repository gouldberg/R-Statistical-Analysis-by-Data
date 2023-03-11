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
# Bradley Terry Trees
# ------------------------------------------------------------------------------

evs <- na.omit(EuropeanValuesStudy)



# ----------
library(psychotree)


bt <- bttree(paircomp ~ gender + eduage + birthyear + marital + employment + income + country2,
             data = evs, alpha = 0.01)



bt

summary(bt)



# ----------
plot(bt, abbreviate = 2)



# ----------
# worth/item parameters in terminal nodes
itempar(bt)


# -->
# node 9 and 13 are similar, but node 13 has low parameter in "freedom"



