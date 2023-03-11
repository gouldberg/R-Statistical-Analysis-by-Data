setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "mice", "mitools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Estimate propensity scores using random forest
#   - Ensemble methods, which combine the results of several classification or regression trees, have been shown to perform well for
#     propensity score estimation and do not suffer from the limitations of classification trees.
#   - One of limitations of recursive partitioning algorithms is that it can artificially prefer numerical variables, variables with more categories, and
#     even variables with more missing data.
#     This problem can be prevented by implementing the random forest algorithm with sampling without replacement.
#
#   - Conditional inference trees estimate a regression relationship by binary recursive partitioning in a conditional inference framework.
#     Roughly, the algorithm works as follows:
#        1) Test the global null hypothesis of independence between any of the input variables and the response (which may be multivariate as well).
#           Stop if this hypothesis cannot be rejected. Otherwise select the input variable with strongest association to the resonse.
#           This association is measured by a p-value corresponding to a test for the partial null hypothesis of a single input variable and the response.
#        2) Implement a binary split in the selected input variable.
#        3) Recursively repeate steps 1) and 2).
# ------------------------------------------------------------------------------

psFormula



# ----------
# fit random forest with settings to produce results
# that are not biased towards continuous variables and categorical variables with many categories.

set.seed(2014)


# Function cforest_unbiased returns the settings suggested for the construction of unbiased random forests
# (teststat = "quad", testtype = "Univ", replace = FALSE)
# by Strobl et al. (2007) and is the default
mycontrols <- cforest_unbiased(ntree = 1000, mtry = 5)



###### THIS TAKES TIME !!!! --  10 min.
# The base year sampling weights bystuwt are specified with the weights argument and used by the random forest algorithm
# to sample observations for each bootstrapped sample based on probabilities equal to the weights divided by the mean of the weights.
# The recommended number of covariates per tree for classification trees is the square root of the number of predictors:
# Given 30 covariates were used, sqrt(30) = 5.48

mycforest <- cforest(psFormula, data = imputation1, weights = imputation1$bystuwt, controls = mycontrols)



# ----------
# plot first ensemble
pt <- prettytree(mycforest@ensemble[[1]], names(mycforest@data@get("input"))) 

nt <- new("BinaryTree") 

nt@tree <- pt 

nt@data <- mycforest@data 

nt@responses <- mycforest@responses 

plot(nt, type="simple")



# ----------
# prediction:
###### THIS TAKES TIME !!!! --  35 min.
# obtain a list of predicted probabilities

pScoresRf <- predict(mycforest, type="prob")




# ----------
# organize the list into a matrix with two columns for the probability of being in treated and control groups.
# keep only the second column, which are the propensity scores.

imputation1$pScoresRf <- matrix(unlist(pScoresRf),,2, byrow=T)[,2]



# save results
# save(list=c("imputation1", "allImputations"), file="imputed_and_estiamted_propensity_score.Rdata", compress=T)



