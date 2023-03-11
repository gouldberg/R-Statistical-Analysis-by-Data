setwd("//media//kswada//MyFiles//R//magazine_prices")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Magazine Prices
# ------------------------------------------------------------------------------

data("MagazinePrices", package = "pder")


str(MagazinePrices)


dim(MagazinePrices)


car::some(MagazinePrices)



# ------------------------------------------------------------------------------
# Conditional logit model
#   - 3 year magazine fixed effects are removed.
# ------------------------------------------------------------------------------

library(survival)


logitC <- clogit(change ~ length + cuminf + cumsales + strata(id), data = MagazinePrices,
              subset = included == 1)



summary(logitC)



# -->
# Note that the coefficient of the lenght of the period since the last price change has the expected positive sign
# and is significant only for the conditional logit model.



# ----------
library(textreg)


screenreg(list(logitS = logitS, logitD = logitD, logitC = logitC), digits = 3)



