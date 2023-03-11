setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ------------------------------------------------------------------------------
# Logistic regression for binomial (proportion) responses
# ------------------------------------------------------------------------------

lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)


# Note that this summary shows not test by deviance,
# but test by z-value (= beta / se(beta)), which is approximately normally distributed.
summary(lmod)


# shortned summary
faraway::sumary(lmod)

