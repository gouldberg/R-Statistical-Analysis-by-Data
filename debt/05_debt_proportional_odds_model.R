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
# Proportional Odds Model
# ------------------------------------------------------------------------------

debt$ccarduse <- factor(debt$ccarduse, levels = c(1, 2, 3), ordered = TRUE)


library(MASS)


pomod <- polr(ccarduse ~ ., data = na.omit(debt))


pomod


summary(pomod)



# ----------
c(deviance(mmod), mmod$edf)

c(deviance(pomod), pomod$edf)


pchisq(deviance(pomod) - deviance(mmod), mmod$edf - pomod$edf, lower = FALSE)



# -->
# The proportiona odds model uses fewer parameters, but does not fit quite as well.




# ------------------------------------------------------------------------------
# Model with least significant variable:  house
# ------------------------------------------------------------------------------


pomod_l <- polr(ccarduse ~ house, data = debt, na.action = na.exclude)


summary(pomod_l)



# -->
# house is now significant ..  this variable is correlated by ccarduse ?


table(debt$ccarduse, debt$house, useNA = "always")




# ------------------------------------------------------------------------------
# Model selection by AIC-based
# ------------------------------------------------------------------------------

pomodi <- step(pomod, direction = "both")


summary(pomodi)



# ----------
# Likelihood ratio test to compare the models
c(deviance(mmodi), mmodi$edf)


c(deviance(pomodi), pomodi$edf)


pchisq(deviance(pomodi) - deviance(mmodi), mmodi$edf - pomodi$edf, lower = FALSE)



# -->
# We see that the simplification to just income is justifiable.

