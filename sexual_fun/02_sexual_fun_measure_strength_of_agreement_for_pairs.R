setwd("//media//kswada//MyFiles//R//sexual_fun")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Sexual Fun
# ------------------------------------------------------------------------------
data("SexualFun", package = "vcd")

data <- SexualFun

data


# ----------
sieve(data, shade=TRUE)

# --> In each row the diagonal entry is not always the largest, though it appears that the partners tend to agree more often
# either responds "Almost always"



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

Kappa(data, weights = "Fleiss-Cohen")

confint(Kappa(data, weights = "Fleiss-Cohen"))


# --> 
# Note that Hypothesis test does not reject by unweighted kappa, but weighted kappa rejects.



# ------------------------------------------------------------------------------
# Visualize observer agreement: agreement chart
#
#  - More importantly, agreementplot shows the pattern of disagreement when agreement is less than perfect.
#
# black squares:  each of size show observed agreement
# visual impression of the strength of agreement is given by B = area of dark squares / area of rectangles
# ------------------------------------------------------------------------------

agreementplot(data, main = "Unweighted", weights = 1)

agreementplot(data, main = "weighted")



# ------------------------------------------------------------------------------
# Weighted measure of agreement
#  = weighted sum of areas of agreement / area of rectangles
# ------------------------------------------------------------------------------

B <- agreementplot(data)

unlist(B)[1:2]


# -->
# Weighted measure of agreement is 0.49817, indicating a stronger degree of agreement when 1-step disagreements are included.







