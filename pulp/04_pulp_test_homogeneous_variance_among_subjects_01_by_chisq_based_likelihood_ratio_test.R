setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ------------------------------------------------------------------------------
# Null Hypothesis:  the variance between the operators is zero
#  - In the mixed effect model where the operators are treated as random, the hypothesis that this variance is zero claims that
#    there is no differences between operators in the population.
#    This is a stronger claim than the fixed effect model hypothesis about just the four chosen operators.
#
# Likelihood Ratio Test
# ------------------------------------------------------------------------------

nullmod <- lm(bright ~ 1, data = pulp)



# ----------
( lrstat <- as.numeric(2 * (logLik(smod) - logLik(nullmod))) )

pvalue <- pchisq(lrstat, 1, lower = FALSE)

data.frame(lrstat, pvalue)



# -->
# The pvalue is now well above the 5% significance level.
# We cannot say that this result is necessarily wrong, but the use of the X^2 approximation does cause us to doubt the result.



