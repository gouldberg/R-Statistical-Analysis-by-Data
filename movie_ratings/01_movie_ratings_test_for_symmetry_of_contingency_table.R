setwd("//media//kswada//MyFiles//R//movie_ratings")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Movie Ratings
#  - Agresti and Winner (1997) gave the data on the ratings of 160 movies by the reviewers Gene Siskel and Roger Ebert
#    for the period from April 1995 through September 1996. The rating categories were Con ("thumbs down"), Mixed, and Pro ("thumbs up")
# ------------------------------------------------------------------------------
data <- matrix(c(24, 8, 13, 8, 13, 11, 10, 9, 64), nrow=3, ncol=3)

data <- as.table(data)

dimnames(data) <- list(Siskel = c("Con", "Mixed", "Pro"), Ebert = c("Con", "Mixed", "Pro"))

data



# ------------------------------------------------------------------------------
# sieve digram
# ------------------------------------------------------------------------------
sieve(data, shade=TRUE)

# plot(data, shade=TRUE)



# ------------------------------------------------------------------------------
# Computing Kappa, and its confidence intervals for agreement for paris responses
#
# Cohen's kappa (k)
#  - commonly used measure of agreement that compares the observed agreement to agreement expected by chance if the two observer's ratings
#    were independent.
#  - Cohen's kappa (k) is the ratio of the difference between acutal agreement and chance agreement, P0 - Pc, to the maximum value this difference could obtain
#    = (P0 - Pc) / (1 - Pc)
#    When agreement is perfect, k = 1; when agreement is no better than would be obtained from statistically independent ratings, k = 0
#    k could conceivably be negative, but this rarely occurs in practice.
#    The minimum possible value depends on the marginal totals.
#  - For larger samples, k has an approximate normal distribution when H0: k = 0 is true and its standard error is given by sigma(k) = ...
#    Hence, it is common to conduct a test of H0: k = 0 by referring z = k / sigma(k) to a unit normal distibution.
#
# Weighted Kappa
#  - The origianl (unweighted) k only counts strict agreement (the same category is assigned by both observers).
#    A weighted version of k may be used when one wishes to allow for partial agreement.
#    For example, exact agreements might be give full weight, while a one-category difference might by given a weight of 1/2.
#    Weighted k uses weights, 0 - 1, for each cell in the table, with weight = 1 for the diagonal cells.
#  - For ans R * R table, two commonly used patterns of weights are those based on equal spacing of weights for a near-match,
#    and Fleiss-Cohen weights based on an inverse-square spacing.
#    The Fleiss-Cohen weights attach greater importance to near disagreements
# ------------------------------------------------------------------------------
Kappa(data)


# ----------
# weighted kappa = 0.458
Kappa(data, weights = "Fleiss-Cohen")

confint(Kappa(data, weights = "Fleiss-Cohen"))


# ----------
# for reference:  Cramer's V = 0.376:  not appropriate to test agreement (strength of agreement is not strength of association)
assocstats(data)



# ------------------------------------------------------------------------------
# Visualize observer agreement: agreement chart
#
#  - More importantly, agreementplot shows the pattern of disagreement when agreement is less than perfect.
#
# black squares:  each of size show observed agreement
# visual impression of the strength of agreement is given by B = area of dark squares / area of rectangles
# ------------------------------------------------------------------------------
# Ratings are almost symmetry for Siskel and Evert.

agreementplot(data, main = "Unweighted", weights = 1)

agreementplot(data, main = "weighted")




# ------------------------------------------------------------------------------
# Perform McNemar's chi-squared test for symmetry of rows and columns in a 2-dimensional contingency table
#  - The null is that the probabilities of being classified into cells [i,j] and [j,i] are the same
# ------------------------------------------------------------------------------
# not rejecting the null hypothesis --> those ratings are symmetry
mcnemar.test(data)






