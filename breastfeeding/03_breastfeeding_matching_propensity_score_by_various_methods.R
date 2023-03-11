setwd("//media//kswada//MyFiles//R//breastfeeding")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# One-to-One greedy matching with replacement and with caliper to estimate the ATT
#   - Greedy matching consists of choosing each treated case and searching for the best available match among the untreated cases
#     without accounting for the quality of the match of the entire treated sample.
#   - Greedy matching works well for estimating the ATT when the number of treated cases is substantially smaller than the number of untreated cases
#     available for matching (i.e., the ratio of treated to untreated sample sizes is small) and there is common support for all treated cases.
#   - When performing greedy matching with replacement, the order of matches does not matter, while greedy matching without replacement
#     will produce different results depending on the  order in which cases are matched.
#     Matching with replacement performs better when the number of available matches is small, but the difference between these methods tends to
#     disappear as the size of the pool of available matches increases.
#
#   - One-to-One matching does not result in a substantial drop of power, because the power is driven by the size of the smallest group,
#     and the increased homogeneity of the sample increases power.
# ------------------------------------------------------------------------------

# searching for the nearest untreated observation within a caliper of 0.25 standard deviations of each treated observation.
# The argument m.order = "largest" specifies that matching should start from the treated case with the largest propensity score.
greedyMatching <- matchit(psFormula, distance = data$logitPScores, 
                          data = data, m.order = "largest",
                          method = "nearest", ratio = 1, replace = T, caliper = 0.25)


# diagnose covariate balance
( balance.greedyMatching <- summary(greedyMatching, standardize = T) )


# obtain the summary of balance aftdr matching 
summary(abs(balance.greedyMatching$sum.matched$"Std. Mean Diff."))

table(abs(balance.greedyMatching$sum.matched$"Std. Mean Diff.") > 0.1)



# ------------------------------------------------------------------------------
# variable ratio greedy matching with replacement and with caliper to estimate the ATT
#   - If each single treatment case is matched to one to several untreated cases (i.e., the number of matches varies across treated cases), 
#     the method is known as variable ratio matchin or one-to-many matching.
#     Research has shown that variable ratio matching removes more bias than one-to-one matching.
#     Variable ratio matching is particularly useful if the number of untreated group cases is much larger than the number of treatment cases.
#     Variable ratio matching is known to outperform one-to-one matching for estimating the treatment effect in general conditions, but the
#     difference in performance between these methods decreases as the number of available matches increases.
#------------------------------------

data$childCare <- data$childCare==1 #Maching requires the treatment indicator to be logical (TRUE/FALSE)


# Y, Tr, and X specify the outcome, the treatment, and the propensity scores, respectively.
# M = 1: one-to-one matching
# ties = TRUE: allowing ties and therefore if more than one case is an adequate match to another case, all matches are included --> variable ratio matching
# caliper = 0.25: maximum allowed distance between a treated and an untreated case to be equal to 0,25 SD.
greedyMatching2 <- with(data,
                        Match(Y = C0338600, Tr = childCare, X = logitPScores,
                              estimand = "ATT", M = 1,
                              caliper = 0.25, replace = TRUE, ties = TRUE))


# evaluate covaraite balance
balance.greedyMatching2 <- MatchBalance(psFormula, data = data, match.out = greedyMatching2, ks = F, paired = F)

balance.greedyMatching2After <- unlist(balance.greedyMatching2$AfterMatching)



# summarize only the standized mean differences (they have been multiplied by 100, so I divided by 100) 
# see details in ?balanceUV
summary(abs(balance.greedyMatching2After[names(balance.greedyMatching2After)=="sdiff"]/100))

table(abs(balance.greedyMatching2After[names(balance.greedyMatching2After)=="sdiff"]/100) > 0.1)



# ------------------------------------------------------------------------------
# Variable ratio genetic matching with replacement (no caliper) based on covariates and the Propensity score to estimate the ATT
#   - Genetic matching minimizes a multivariate weighted distance on covariates between treated and untreated cases, where a genetic algorithm
#     is used to choose weights that optimize postmatching covariate balance.
#     The distance minimized by the genetic matching algorithm is the generalized Mahalanobis distance (GMD)
#     The GMD can be understood as a weighted average effect size between treated and untreated groups across all covariates.
# ------------------------------------------------------------------------------

# create a dataset of covariates in numeric format
covariateData <- as.matrix(data$logitPScores)

for (c in covariateNames) {
  covariateData <- cbind(covariateData, as.numeric(as.matrix(data[,c]))) }


colnames(covariateData) <- c("logitPScores", covariateNames)



# ----------
# GenMatch function uses the genetic algorithm to obtain weights that optimize covariate balance.
# For each generation (i.e., iteration), the genetic algorithm sets the weights in W (in GMD) to initial values (the default initial value is 1)
# and generates as many W matrices as the specified population size in the pop.size argument.
# Because genetic matching optimizes covariate balance asymptotically, it is important to specify a large population size for the genetic optimization.

# The default loss function is specified in fit.func = "pvals", which consists of the maximum of p values from Kolmogorov-Smirnov tests and paired t tests
# for all covariates. While using p values for covariate balance assessment is problematic because it depends on sample size,
# it is a good choice for defining the fit function because the sample size is fixed within the optimization.
geneticWeights <- GenMatch(Tr = data$childCare, X = covariateData, 
                           pop.size = 1000, fit.func = "pvals", 
                           estimand = "ATT", replace = T, ties = T)

geneticWeights


geneticMatching <- Match(Y = data$C0338600, Tr = data$childCare, X = covariateData,
                         Weight.matrix = geneticWeights, estimand = "ATT", 
                         M = 1,  replace = TRUE, ties = TRUE)


# ----------
# evaluate covaraite balance
balance.geneticMatching <- MatchBalance(psFormula, data = data, match.out = geneticMatching, ks = F, paired = F)

balance.geneticMatchingAfter <- unlist(balance.geneticMatching$AfterMatching)



# summarize only the standized mean differences (they have been multiplied by 100, so I divided by 100)
# see details in ?balanceUV
summary(abs(balance.geneticMatchingAfter[names(balance.geneticMatchingAfter)=="sdiff"]/100))

table(abs(balance.geneticMatchingAfter[names(balance.geneticMatchingAfter)=="sdiff"]/100) > 0.1)



# ------------------------------------------------------------------------------
# Variable ratio genetic matching for estimating the ATT (no caliper) with one to many matching with replacement based on the propensity score only
# by "matchit" function
# ------------------------------------------------------------------------------

geneticMatching2 <- matchit(psFormula, distance = data$logitPScores, 
                            data = data, method = "genetic", pop.size = 1000,
                            fit.func = "pvals", 
                            estimand = "ATT", replace = T, ties = T)


# diagnose covariate balance
( balance.geneticMatching2 <- summary(geneticMatching2, standardize = T) )


# obtain the summary of balance aftdr matching 
summary(abs(balance.geneticMatching2$sum.matched$"Std. Mean Diff."))

table(abs(balance.geneticMatching2$sum.matched$"Std. Mean Diff.") > 0.1)



# ------------------------------------------------------------------------------
# Optimal one-to-one matching without replacement
#   - Optimal matching was proposed by Rosenbaum (1989) as a solution to the problem that greedy matching does not guarantee matches with
#     the minimum total distance between treated and matched groups.
#     Optimal matching produces matches that attain minimal total distances by using network flow optimization methods.
#   - Rosenbaum cautioned that optimal one-to-one and one-to-k matching only guarantees minimum total distance given the constraint of the
#     matching ratio desired.
#   - Optimal one-to-one matching is expected to outperform one-to-one greedy matching, but the differences in match quality are
#     small when many matches are available. However, when the treated to untreated ratio is large, one-to-one optimal matching is noticeably better
#     than one-to-one greedy matching.
# ------------------------------------------------------------------------------
library(optmatch)

optimalMatching <- matchit(psFormula, distance = data$logitPScores, 
                           data = data, method = "optimal", ratio = 1)


# diagnose covariate balance
( balance.optimalMatching <- summary(optimalMatching, standardize=T) )


# obtain the summary of balance after matching 
summary(abs(balance.optimalMatching$sum.matched$"Std. Mean Diff."))

table(abs(balance.optimalMatching$sum.matched$"Std. Mean Diff.") > 0.1)



# ------------------------------------------------------------------------------
# Full matching
#   - Full matching matches each treated case to at least one untreated case and vice versa, without replacement.
#     Therefore, this procedure can be viewed as a propensity score stratification where the number of strata containing at least one treated
#     and one untreated observation is maximized.
#   - Differently from one-to-one matching with replacement and variable ratio matching with replacement, the matched sets never overlap and observations are not discarded,
#     which allows the estimation of treatement effects and standard errors with methods appropriate for finely stratified samples.
#     Full matchin is particularly helpful when there are large differences in the distributions of propensity scores between treated and untreated
#     (assuming common support is still adequate)
#     Full matching has been found to perform better than one-to-many greedy matching in terms of distance within matched sets as well as covariate balance,
#     especially when the number of covariates is large.
# ------------------------------------------------------------------------------
library(optmatch)

fullMatching <- matchit(psFormula, distance = data$logitPScores, 
                        data = data, method = "full")



# diagnose covariate balance
( balance.fullMatching <- summary(fullMatching, standardize = T) )


# obtain the summary of balance after matching 
summary(abs(balance.fullMatching$sum.matched$"Std. Mean Diff."))

table(abs(balance.fullMatching$sum.matched$"Std. Mean Diff.") > 0.1)




# ------------------------------------------------------------------------------
# Evaluation of covariate balance
# ------------------------------------------------------------------------------

# There are 31 covariates in the propensity score mode, but covariance balance is evaluated for the propensity score,
# continuous covariates, and levels of categorical covariates, so the total number of covariate balance measures
# obtained is 42.

# summary(., standardize = T): can be used for greedy matching, genetic matching, optimal matching and full matching
# Match::MatchBalance(): can be used for genetic matching, and greedy matching


# -->
# None of the matching methods produced absolute standardized mean differences lower than 0.1 for all covariates, 
# but three produced differences lower than 0.25 standard deviations for all covariates.



# ----------
# 1. One-to-One greedy matching with replacement and with caliper to estimate the ATT
( balance.greedyMatching <- summary(greedyMatching, standardize = T) )
( balance.greedyMatching_2 <- MatchBalance(psFormula, data = data, match.out = greedyMatching, ks = F, paired = F) )
# ( balance.greedyMatching_2After <- unlist(balance.greedyMatching_2$AfterMatching) )
summary(abs(balance.greedyMatching$sum.matched$"Std. Mean Diff."))
table(abs(balance.greedyMatching$sum.matched$"Std. Mean Diff.") > 0.1)



# ----------
# 2. variable ratio greedy matching with replacement and with caliper to estimate the ATT
( balance.greedyMatching <- summary(greedyMatching2, standardize = T) )
balance.greedyMatching2 <- MatchBalance(psFormula, data = data, match.out = greedyMatching2, ks = F, paired = F)
balance.greedyMatching2After <- unlist(balance.greedyMatching2$AfterMatching)
summary(abs(balance.greedyMatching2After[names(balance.greedyMatching2After)=="sdiff"]/100))
table(abs(balance.greedyMatching2After[names(balance.greedyMatching2After)=="sdiff"]/100) > 0.1)



# ----------
# 3. Variable ratio genetic matching with replacement (no caliper) based on covariates and the Propensity score to estimate the ATT
( balance.geneticMatching_2 <- summary(geneticMatching, standardize = T) )
balance.geneticMatching <- MatchBalance(psFormula, data = data, match.out = geneticMatching, ks = F, paired = F)
balance.geneticMatchingAfter <- unlist(balance.geneticMatching$AfterMatching)
summary(abs(balance.geneticMatchingAfter[names(balance.geneticMatchingAfter)=="sdiff"]/100))
table(abs(balance.geneticMatchingAfter[names(balance.geneticMatchingAfter)=="sdiff"]/100) > 0.1)



# ----------
# 4. Variable ratio genetic matching for estimating the ATT (no caliper) with one to many matching with replacement based on the propensity score only
( balance.geneticMatching2 <- summary(geneticMatching2, standardize = T) )
summary(abs(balance.geneticMatching2$sum.matched$"Std. Mean Diff."))
table(abs(balance.geneticMatching2$sum.matched$"Std. Mean Diff.") > 0.1)




# ----------
# 5. Optimal one-to-one matching without replacement
( balance.optimalMatching <- summary(optimalMatching, standardize=T) )
# balance.geneticMatching2 <- MatchBalance(psFormula, data = data, match.out = optimalMatching, ks = F, paired = F)
summary(abs(balance.optimalMatching$sum.matched$"Std. Mean Diff."))
table(abs(balance.optimalMatching$sum.matched$"Std. Mean Diff.") > 0.1)



# ----------
# 6. Full matching
( balance.fullMatching <- summary(fullMatching, standardize = T) )
summary(abs(balance.fullMatching$sum.matched$"Std. Mean Diff."))
table(abs(balance.fullMatching$sum.matched$"Std. Mean Diff.") > 0.1)
