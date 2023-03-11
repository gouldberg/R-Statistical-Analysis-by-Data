# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret", "pROC", "klaR", "e1071")
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
# Set train control
# ------------------------------------------------------------------------------
# This control object will be used across multiple models so that the data splitting is consistent
# Train must know exactly which samples to use when estimating parameters.
# The "index" argument to trainControl identifies these samples. For any sampling method, a set of holdout samples can be exactly specified.
# For example, with 10-fold cross-validation, the exact samples to be excluded for each of the 10-folds are identified with this option.
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     index = list(TrainSet = pre2008),
                     savePredictions = TRUE)



# ------------------------------------------------------------------------------
# Nonlinear Classification Models:  Naive Bayes
#  - The naive Bayes model simplifies the probabilities of the predictor values by assuming that all of the predictors are independent of the others.
#    This is an extremely strong assumption, but the assumption of independence yields a significant reduction in the complexity of the calculations.
#    For example, to calculate the conditional probability, we would use a product of the probability densities for each individual predictor.
#    The unconditional probability results in a similar formula when assuming independence.
#  - To estimate the individual probabilities, an assumption of normality might be made for continuous predictors (using the sample mean and variance from the training set).
#    Other methods, such as nonparametric kernel density estimators (Hardle et al. 2004), can more flexibly estimate the probability densities.
#    For categorical predictors, the probability distribution can be determined with the observed frequencies in the training set data.
#  - Posterior Probability = Prior Probability of Outcome * Conditional Probability of Outcome for the Predictor values / Probability of the predictor values
#    The prior probabilty allows the modeler to tilt the final probability towards one or more classes.
#  - Advantage of the naive Bayes model
#       - can be computed quickly, even for large training sets.
#       - despite such a strong assumption, the model performs competitively in many cases.
#       - Bayes' Rule is essentially a probability statement
#  - The nonparametric densities can produce more flexible probability estimates.
#  - An obvious issue, especially for small samples sizes, occurs when one or more frequencies are zero. One method for avoiding this issues is to use a 
#    Laplace correction or Laplace smoothing where the same correction factor, usually between one and two, is added to the numerator.
#    For the denominator, the frequencies are increase by the correction factor times the number of values of the predictor.
#    (when corerction factor is 1 and the number of values of the predictor is 7 (such as weekday), the frequencies for denominator is increased by 7)
# ------------------------------------------------------------------------------

# The two main functions for fitting the naive Bayes models in R are naiveBayes in the e1071 package and NaiveBayes in the klaR package.
# Both offer Laplace corrections, but the version in the klaR package has the option of using conditional density estimates that are more flexible.
# Feeding these models binary dummy variables (instead of a factor variable) is problematic since the individual categories will be treated as
# numerical data and the model will estimate the probability density function from a continuous distribution, such as the Gaussian.


factorPredictors <- names(training)[names(training) != "Class"]
factorPredictors <- factorPredictors[!grepl("Sponsor[0-9]", factorPredictors)]
factorPredictors <- factorPredictors[!grepl("SponsorUnk", factorPredictors)]
factorPredictors <- factorPredictors[!grepl("ContractValueBand[A-Z]", factorPredictors)]
factorPredictors <- factorPredictors[!grepl("GrantCat", factorPredictors)]
factorPredictors <- factorPredictors[!(factorPredictors %in% levels(training$Month))]
factorPredictors <- factorPredictors[!(factorPredictors %in% levels(training$Weekday))]



# ----------
# Create factor versions of some of the predictors so that they are treated as categories and not dummy variables
# Some predictors are already stored as factors
factors <- c("SponsorCode", "ContractValueBand", "Month", "Weekday")


# Get the other predictors from the reduced set
nbPredictors <- factorPredictors[factorPredictors %in% reducedSet]
nbPredictors <- c(nbPredictors, factors)
nbPredictors <- nbPredictors[nbPredictors != "SponsorUnk"]



# ----------
tmp <- colnames(training)
setdiff(tmp, c("Class", nbPredictors))



# ----------
# Leek only those that are needed
nbTraining <- training[, c("Class", nbPredictors)]
nbTesting <- testing[, c("Class", nbPredictors)]



# ----------
# Loop through the predictors and convert some to factors
for(i in nbPredictors)
{
  if(length(unique(training[,i])) <= 15)
  {
    nbTraining[, i] <- factor(nbTraining[,i], levels = paste(sort(unique(training[,i]))))
    nbTesting[, i] <- factor(nbTesting[,i], levels = paste(sort(unique(training[,i]))))
  }
}



set.seed(476)



# ----------
# usekernel: TRUE for a kernel density estimate, FALSE for a normal density estimate
# fL: factor for Laplace correction, default factor is 0 (no correction)
nbGrid = data.frame(expand.grid(usekernel = c(TRUE, FALSE), fL = c(1, 2), adjust = c(TRUE, FALSE)))

nBayesFit <- train(x = nbTraining[,nbPredictors], y = nbTraining$Class, method = "nb", metric = "ROC",
                   tuneGrid = nbGrid, trControl = ctrl)


nBayesFit



# ------------------------------------------------------------------------------
# model by e1071 package
# ------------------------------------------------------------------------------

nBayesFit <- e1071::naiveBayes(Class ~., data = nbTraining[pre2008,], usekernel = TRUE, fL = 2)

nBayesFit


pred <- predict(nBayesFit, newdata = head(nbTesting))

