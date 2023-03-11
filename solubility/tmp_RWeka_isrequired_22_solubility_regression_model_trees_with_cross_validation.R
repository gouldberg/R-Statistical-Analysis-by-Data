setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "MASS", "RWeka")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  solubility
# ------------------------------------------------------------------------------
data("solubility", package = "AppliedPredictiveModeling")

dim(solTrainX)

names(solTrainX)

head(solTestY)

str(solTrainX)


fingerprints <- grep("FP", names(solTrainXtrans))
length(fingerprints)


# rpart only uses formulas, so we put the predictors and outcome into a common data frame first.
trainData <- solTrainXtrans
trainData$y <- solTrainY



# ------------------------------------------------------------------------------
# Create folds explicitly
# ------------------------------------------------------------------------------
set.seed(100)


# create folds explicitly, default is 10 folds
indx <- createFolds(solTrainY, returnTrain = TRUE)
indx



# ------------------------------------------------------------------------------
# Set train control
# ------------------------------------------------------------------------------
ctrl <- trainControl(method = "cv", index = indx)



# ------------------------------------------------------------------------------
# Regression Trees and Rule-Based Models:  Regression Model Trees
#   - One limitation of simple regression trees is that each terminal node uses the average of the training set outcomes in that node for prediction.
#     As a consequence, these models may not do a good job predicting samples whose true outcomes are extremely high or low.
#   - One approach to dealing with this issus is to use a different estimator in the terminal nodes.
#     Here we focus on the "model tree" appraoch described in Quinlan (1992) called "M5", which is similar to regression trees except:
#       - The spltting criterion is different
#       - The terminal nodes predict the outcome using a linear model (as opposed to the simple average)
#       - When a sample is predicted, it is often a combination of the predictions from different models along the same path through the tree.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Tune the model tree (using method = "M5") by caret::train
# ------------------------------------------------------------------------------
# Tune the model tree.
# Using method = "M5" actually tunes over the tree- and rule-based versions of the model. M = 10 is also passed
# in to make sure that there are larger terminal nodes for the regression models.

set.seed(100)

m5Tune <- train(x = solTrainXtrans, y = solTrainY, method = "M5", trControl = ctrl, control = Weka_control(M = 10))

m5Tune

plot(m5Tune)

m5Tune$finalModel

plot(m5Tune$finalModel)


# Show the rule-based model too
ruleFit <- M5Rules(y~., data = trainData, control = Weka_control(M = 10))

ruleFit

