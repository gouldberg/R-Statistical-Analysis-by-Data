setwd("//media//kswada//MyFiles//R//rwdq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RWDQ (Work Design Questionnaire R Package Authors)
# ------------------------------------------------------------------------------

data("RWDQ", package = "MPsychoR")

str(RWDQ)


car::some(RWDQ)



# ------------------------------------------------------------------------------
# Two-parameter logistic model (2-PL)
#   - more relevant than the onw-parameter logistic model.
#   - It estimates an item discrimination parameter alpha(i) for each item individually, which allows the ICCs to cross
#   - P(X(vi) = 1) = exp(alpha(i) * (theta(v) - beta(i))) / { 1 + exp(alpha(i) * (theta(v) - beta(i))) }
#   - Compared to the Rasch model, it relaxes the assumption of parallel ICCs through alpha(i), while unidimensionality and local independence stil need to hold/
# ------------------------------------------------------------------------------

library(ltm)


# Note that z1 is a generic placeholder for the single latent dimension
fit2pl1 <- ltm(RWDQ ~ z1)


fit2pl1



# ----------
# The item parameters
head(coef(fit2pl1))



# -->
# The first column contains the item difficulty parameters beta(i),
# A large beta(i) simply means that the item is located at the upper end of the knowledge characteristics continuum.
# That is, it allows us to discriminate among persons knowledge characteristics at its lower end and, correspondingly,
# discriminates among persons with low knowledge characteristic.

# The second column shows the item discrimination parameters alpha(i) which reflect the varying ICC slopes.

# The item location parameter of item 22 is unreasonably low, compared to the other parameters.
# Reasons for such a heavily outlying parameter are that either the algorithm did not converge or the item violates an assumption.




# ------------------------------------------------------------------------------
# Eliminate wdq_22 and refit 2-PL model
# ------------------------------------------------------------------------------

RWDQ1 <- RWDQ[, -1]

fit2pl2 <- ltm(RWDQ1 ~ z1)



# ----------
head(coef(fit2pl2))


# -->
# All item parameters look reasonable now.



# ------------------------------------------------------------------------------
# itemfit check by computing a X^2-based itemfit statistic called Q1
#   - In general, a significant p-value suggests that the item does not fit.
#     But based on the fact that the Q1 statistic exhibits inflated Type I error rates,
#     we do not have to use these p-values as our only criterion to keep or eliminate items.
# ------------------------------------------------------------------------------

item.fit(fit2pl2)


# -->
# The results suggest that the entire set of items fits; none of the p-values is significant.
# Thus, no further item elimination steps are needed.

