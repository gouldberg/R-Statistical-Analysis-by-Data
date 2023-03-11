setwd("//media//kswada//MyFiles//R//hsb")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hsb
# ------------------------------------------------------------------------------

data("hsb", package = "faraway")


str(hsb)


head(hsb)



# ------------------------------------------------------------------------------
# multinominal logit model
# ------------------------------------------------------------------------------

library(nnet)


mmod <- multinom(prog ~ gender + race + ses + schtyp + read + write + math + science + socst, data = hsb)


summary(mmod)



# -->
# science have different coefficients (same as LDA analysis)



# ------------------------------------------------------------------------------
# Model selection
# ------------------------------------------------------------------------------

mmodi <- step(mmod, direction = "both")


summary(mmodi)



# -->
# gender, race, read, write are dropped.



# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------


anova(mmodi, mmod)



