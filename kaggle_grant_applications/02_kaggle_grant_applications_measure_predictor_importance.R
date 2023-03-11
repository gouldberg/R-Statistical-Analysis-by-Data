rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("plyr", "dplyr", "caret", "reshape2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ----------
library(doMC)
cores <- 20
registerDoMC(cores)



# ----------
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/training")
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/testing")
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/fullSet")
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/reducedSet")
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/pre2008")



# ------------------------------------------------------------------------------
# Compute several binary statiscits for binary predictors
# ------------------------------------------------------------------------------
# For grant application data, there are 226 binary predictors with at least 25 entries in each cell of the 2 * 2 table between the predictor and outcome.
# We pick 3 binary predictors
dataSubset <- training[pre2008, c("Sponsor62B", "ContractValueBandUnk", "RFCD240302")]

table(dataSubset$Sponsor62B)
table(dataSubset$ContractValueBandUnk)
table(dataSubset$RFCD240302)



# ----------
# This is a simple function to compute several statistics for binary predictors
tableCalcs <- function(x, y)
{
  tab <- table(x, y)
  fet <- fisher.test(tab)
  out <- c(OR = fet$estimate,
           P = fet$p.value,
           Gain = attrEval(y ~ x, estimator = "GainRatio"))
}



# lapply() is used to execute the function on each column
tableResults <- lapply(dataSubset, tableCalcs, y = training[pre2008, "Class"])
tableResults <- do.call("rbind", tableResults)

tableResults



# --+
# For contract value band, the probability of grant success is determined when the band is known and unknown.
# The corresponding odds ratio is 6.3, meaning that there is more than a sixfold increase in the odds of success when the band is known.
# Nased on Fisher's exact test to evaluate that the odds ratio is equal to one, the p-value was effectively zero, which indicates that
# levels of contract value band are related to grant success.

# The odds ratio for ContractValueBand is not much different than that of Sponsor62B,
# but the p-value of Fisher's exact test and information gain favor the contract value band predictor.



# ------------------------------------------------------------------------------
# The Relief value
#   - The Relief algorithm (Kira and Rendell 1992) is a generic method for quantifying predictor importance.
#     It was originally developed for classification problems with two classes but has been extended to work across a wider range of problems
#   - It can accomodate continuous predictors as well as dummy variables and can recognize nonlinear relationships between the predictors and the outcome.
#   - It uses random selected points and their nearest neighbors to evaluate each predictor in isolation.
#     For a particular predictor, the score attempts to characterize the separation between the classes in isolated sections of the data.
#     For a randomly selected training set sample, the algorithm finds the nearest samples from both classes (called the "hits" and "misses").
#     For each predictor, a measure of difference in the predictor's values is calculated between the random data point and the hits and misses.
#     The idea is that a predictor that shows a separation between the classes should have hits nearby and missed far away.
#     Larger scores are indicative of important predictors.
#   - For continuous predictors, Kira nad Rendell (1992) suggested the distance between the two points be divided by the overall range of the predictor
#       diff(x, y) = (x - y) / C   where C is a constant for the predictor that scales the difference to be between 0 and 1.
#     For binary data, a simple indicator for equivalence can be used:  diff(x, y) = | x - y |
#
#   - Kononenko (1994) modified algorithm, called ReliefF, to use more than a single nearest neighbors, use a modified difference metric, 
#     and allow for more than two classes as well as missing predictor values. Additionally, Robnik-Sionja and Kononenko (1997) adapted the 
#     algorithm for regression (i.e., numeric outcomes)
# ------------------------------------------------------------------------------
library(CORElearn)

# The permuted Relief scores can be computed using a function from the AppliedPredictiveModeling package. 

permuted <- permuteRelief(x = training[pre2008, c("Sponsor62B", "Day", "NumCI")], y = training[pre2008, "Class"],
                          nperm = 500, estimator="ReliefFequalK", ReliefIterations= 50)


# The original Relief scores:
permuted$observed


# The number of standard deviations away from the permuted mean:
permuted$standardized



# The distributions of the scores if there were no relationship between the predictors and outcomes
histogram(~ value | Predictor, 
          data = permuted$permutations, 
          xlim = extendrange(permuted$permutations$value),
          xlab = "Relief Score")

