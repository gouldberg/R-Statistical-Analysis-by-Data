setwd("//media//kswada//MyFiles//R//rmotivation")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rmotivation
# ------------------------------------------------------------------------------

data("Rmotivation", package = "MPsychoR")


str(Rmotivation)



# ----------
# Here we focus on hybrid motivation only and selct 19 dichotomous items associated with this latent variable

( ind <- grep("hyb", colnames(Rmotivation)) )

HybMotivation <- na.omit(Rmotivation[, ind])

car::some(HybMotivation)



# ------------------------------------------------------------------------------
# Greatest Lower Bound (glb)
#   - Cronbach's alpha is a reasonable lower bound for reliability if the items in a test are essentially tau-equivalent.
#     This implies that each item measures the same latent variable but with possibly different degrees of precision and that the inter-item
#     covariances are constant across item pairs. This is not a very realistic assumption in practice.
#   - If violated, alpha underestimates the reliability, whereas the glb provides a better reliability approximation: glb >= alpha.
# ------------------------------------------------------------------------------


psych::glb(HybMotivation)

psych::glb(HybMotivation)$glb.max

round(alpha.hyb$total[1], 2)


# -->
# We get a glb of 0.89 which is larger than Cronbach's alpha (= 0.82)




# ------------------------------------------------------------------------------
# McDonald's omega
#   - McDonald's omega's computation is based on factor analysis.
#   - omega-h coefficient:  proportion of variance in observed scores accounted for by a single general factor (i.e., a single latent variable)
#     Note that the general factor concept is weaker than unidimensionality:
#     It tells us whether our scale has a dominating single factor and, possibly, some additional, "weaker" subfactors.
#   - omega-t coefficient:  proportion of test variance due to all common factors
# ------------------------------------------------------------------------------

psych::omega(HybMotivation)



# -->
# We get omega-t as 0.85 which is close to Cronbach's alpha.
# Note that in case of unidimensionality, omega-t = alpha.

# For omega-h, we get a value 0.58 which tells us that 58% of the variance in observed scores is due to the general factor and the
# remaining portion is due to possible subfactors of hybrid motivation.
# omega-h is particularly relevant when evaluating the importance and reliability of the general factor of a test.
# Its upper limit is not 1 (as opposed to omega-t)

