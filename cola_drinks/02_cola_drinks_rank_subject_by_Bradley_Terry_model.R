# setwd("//media//kswada//MyFiles//R//cola_drinks")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//cola_drinks")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coke Drinks
#   - A sample of psychology graduate students at the University of Florida mde blind, pairwise preference tests of 3 cola drinks,
#     Coke, Classic Coke and Pepsi
# ------------------------------------------------------------------------------

Cola <- read.table(file = "cola_drinks.dat", header = T, stringsAsFactors = FALSE, sep = "\t")


str(Cola)


Cola


# -->
# For 49 comparisons of Coke and Pepsi, Coke was preferred 29 times



# ------------------------------------------------------------------------------
# Bradley-Terry model to rank subject
#   - The Bradley-Terry model is a logistic model for paired preference data.
#     logit(PI(ij)) = log(PI(ij) / PI(ji)) = beta(i) - beta(j)
#     the probability that subject i is preferred equals 0.5 when beta(i) = beta(j) and exceeds 0.5 when beta(i) > beta(j)
#     One parameter is redundantt and the model has no intercept.
#   - This logistic model is equivalent to the quasi-symmetry model
# ------------------------------------------------------------------------------


fit <- glm(nij / (nij + nji) ~ -1 + Coke + ClassicCoke + Pepsi, family = binomial, weights = nij + nji, data = Cola)


summary(fit)




# -->
# This model fit yields estimated probabilities of preference.

# When Coke is compared to Pepsi, the estimated probability of Coke is preferred is:
# exp(beta2 - beta1)/ { 1 + exp(beta2 - beta1)}  = exp(0.3251) / (1 + exp(0.3251)) = 0.5806



# -->
# For such small data sets, the model smoothing provides estiamtes that are more sensible than the sample proportions.
