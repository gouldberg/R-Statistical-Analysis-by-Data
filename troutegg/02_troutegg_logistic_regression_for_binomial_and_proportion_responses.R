setwd("//media//kswada//MyFiles//R//troutegg")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  troutegg
# ------------------------------------------------------------------------------

data("troutegg", package = "faraway")

str(troutegg)

head(troutegg)



# ------------------------------------------------------------------------------
# Logistic regression for binomial (proportion) responses
# ------------------------------------------------------------------------------

bmod <- glm(cbind(survive, total - survive) ~ location + period, family = binomial, troutegg)


# Note that this summary shows not test by deviance,
# but test by z-value (= beta / se(beta)), which is approximately normally distributed.
summary(bmod)


# shortned summary
faraway::sumary(bmod)





