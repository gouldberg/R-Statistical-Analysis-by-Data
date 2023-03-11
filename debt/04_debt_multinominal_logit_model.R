setwd("//media//kswada//MyFiles//R//debt")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  debt
# ------------------------------------------------------------------------------

data("debt", package = "faraway")


str(debt)


head(debt)



# ------------------------------------------------------------------------------
# multinominal logit model
# ------------------------------------------------------------------------------

library(nnet)


mmod <- multinom(ccarduse ~ ., data = na.omit(debt))


summary(mmod)



# -->
# cigbuy have different coefficients (same as LDA analysis)



# ------------------------------------------------------------------------------
# Model selection
# ------------------------------------------------------------------------------

mmodi <- step(mmod, direction = "both")


summary(mmodi)




# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------


anova(mmodi, mmod)



