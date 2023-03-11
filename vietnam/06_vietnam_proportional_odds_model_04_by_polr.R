setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)

str(Vietnam)


car::some(Vietnam)



# ----------
Vietnam$response <- ordered(Vietnam$response)



# ------------------------------------------------------------------------------
# proportional odds model by polr
# ------------------------------------------------------------------------------

library(MASS)


# polr() can used weights
viet.polr <- polr(response ~ sex + year, data = Vietnam, weights = Freq, Hess = TRUE)

viet.polr

summary(viet.polr)



# -->
# summary() gives the estimated coefficients and intercepts labeled by the cutpoint on the ordinal response,
# It provides standard errors and t-values (beta / SE(beta)), but no significant tests or p-values


# ----------
# Anova() gives appropriate tests
car::Anova(viet.polr)



