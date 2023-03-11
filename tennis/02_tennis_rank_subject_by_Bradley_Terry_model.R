# setwd("//media//kswada//MyFiles//R//tennis")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//tennis")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tennis
#   - The results of matches among five professional tennis players between January 2014 and January 2018.
# ------------------------------------------------------------------------------

Tennis <- read.table(file = "Tennis.dat", header = T, stringsAsFactors = FALSE)


str(Tennis)


Tennis


# -->
# Roger Federer won 6 of the 15 matches that he and Novak Djokovic played.



# ------------------------------------------------------------------------------
# Bradley-Terry model to rank subject (players)
#   - The Bradley-Terry model is a logistic model for paired preference data.
#     logit(PI(ij)) = log(PI(ij) / PI(ji)) = beta(i) - beta(j)
#     the probability that player i wins equals 0.5 when beta(i) = beta(j) and exceeds 0.5 when beta(i) > beta(j)
#     One parameter is redundantt and the model has no intercept.
#   - This logistic model is equivalent to the quasi-symmetry model
# ------------------------------------------------------------------------------


fit <- glm(nij / (nij + nji) ~ -1 + Djokovic + Federer + Murray + Nadal + Wawrinka, family = binomial, weights = nij + nji, data = Tennis)


summary(fit)




# -->
# This model fit yields estimated probabilities of victory.

# When Federer playes Djokovic, the estimated probability of a Federer win is:
# exp(beta2 - beta1)/ { 1 + exp(beta2 - beta1)}  = exp(1.136 - 1.176) / (1 + exp(1.136 - 1.176)) = 0.49



# -->
# For such small data sets, the model smoothing provides estiamtes that are more sensible than the sample proportions.
# For instance, Federer beat Murray in all 5 of their matches, but the model estimates the probability of a Federer victory to be 0.85 rather than 1.00



# ------------------------------------------------------------------------------
# Check whether the difference between two players is statistically significant
# ------------------------------------------------------------------------------


# The formula here has "Djokobvic" in the last to be baseline
fit2 <- glm(nij / (nij + nji) ~ -1 + Federer + Murray + Nadal + Wawrinka + Djokovic, family = binomial, weights = nij + nji, data = Tennis)


summary(fit2)



# -->
# For comapring Federer and Djokovic, beta2 - beta1 = -0.0040 has standard error = 0.417,
# indicating an insignificant difference


# 95% confidence interval for beta2 - beta1 is (-0.864, 0.790)
# This translates to (0.296, 0.688) for the probabilty for Federer to win
confint(fit2)


exp(-0.8641) / (1 + exp(-0.8641))
exp(0.7903) / (1 + exp(0.7903))


