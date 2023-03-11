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

summary(bmod)

faraway::sumary(bmod)


# -->
# All the term except "location", highly significant ...



# ----------
pchisq(deviance(bmod), df.residual(bmod), lower = FALSE)


# -->
# But since this p-value < 0.05, showing that this model does not fit.



# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained  (Nagelkerke)
# ------------------------------------------------------------------------------

# Nagelkerke R^2
n <- bmod$df.null + 1

(1 - exp((bmod$dev - bmod$null)/n)) / (1 - exp(-bmod$null/n))



# ----------
# McFadden's peudo R^2
1 - bmod$deviance / bmod$null.deviance


# -->
# Goodness of Fit is almost 1 ????



