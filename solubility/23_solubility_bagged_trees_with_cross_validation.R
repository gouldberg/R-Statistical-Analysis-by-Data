setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "MASS")
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
# Regression Trees and Rule-Based Models:  Bagged Trees
#   - Bagging, short for bootstrap aggregation, was originally proposed by Leo Breiman and was one of the earliest developed ensemble techniques
#   - Bagging is a general approach that uses bootstrapping in conjunction with any regression model to construct an ensemble.
#     Each model in the ensemble is then used to generate a prediction for a new sample and these m predictions are averaged to give the bagged model's prediction.
#
#     -------------------------
#     Bagging algorithm:
#     for i = 1 to n do
#       generate a bootstrap sample of the original data
#       train and unpruned tree model on this sample
#     end
#     -------------------------
#
#   - Bagging models provide several advantages over models that are not bagged.
#       (1) effectively reduce the variance of a prediction through its aggregation process, making the prediction more stable.
#           On the other hand, bagging stable, lower variance models like linear regression and MARS, offers less improvement in predictive performance.
#       (2) can provide their own internal estimate of predictive performance ("out-of-bag" estimate) that correlates well with either cross-validation estimates or test set estimates.
#   - Experiences show that the most improvement in prediction performance is obtained with a small number of trees (m < 10), but small improvements
#     can still be made using baggin ensembles up to size 50.
#     If performance is not at an acceptable level after 50 bagging iterations, then we suggest trying other more powerfully predictive ensemble methods
#     such as random forests and boosting.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Tune Bagged Trees model by caret::train
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(10)

set.seed(100)

treebagTune <- train(x = solTrainXtrans, y = solTrainY, method = "treebag", nbagg = 50, trControl = ctrl)

treebagTune


# ----------
# Get the variable importance.
treebagImp <- varImp(treebagTune, scale = TRUE)

treebagImp

plot(treebagImp, top = 20)


# -->
# binary variable (fingerprints) less ranks in top 10 than the model by conditional inference trees



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testRes_treebag <- data.frame(obs = solTestY, treebag = predict(treebagTune, solTestXtrans))
testRes_treebag <- testRes_treebag %>% mutate(resid = obs - treebag)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testRes_treebag$obs, testRes_treebag$treebag))

graphics.off()
par(mfrow = c(1,2))
with(testRes_treebag, plot(obs, treebag, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_treebag, plot(treebag, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


# -->
# Better than fitting by CART model
# but does not do a good job predicting samples whose true outcomes are extremely high or low
# due to limitation of simple regression trees in that each terminal mode uses the average of the training set outcomes in that node for prediction



# ------------------------------------------------------------------------------
# Bagged Trees model by ipred package
#   - ipred package contains 2 functions for bagged trees:  bagging uses the formula interface and ipredbagg has the non-formula interface
#   - This functions uses the rpart function and details about the type of tree can be specified by passing rpart.control to the control argument
# ------------------------------------------------------------------------------
# coob = TRUE:  out-of-bag estimate of the error rate should be computed
# ns:  number of sample to draw from the learning sample. by default, the usual bootstrap n out of n with replacement is performed.
library(ipred)

baggedTree <- ipred::ipredbagg(solTrainY, solTrainXtrans, coob = TRUE)

baggedTree2 <- ipred::bagging(y ~ ., data = trainData, coob = TRUE)


baggedTree

baggedTree2



# ------------------------------------------------------------------------------
# Bagged Trees models with conditional inference trees
# ------------------------------------------------------------------------------
library(party)


# the mtry parameter should be the number of predictors (the number of columns minus 1 for the outcome)
bagCtrl <- party::cforest_control(mtry = ncol(trainData) - 1)

ctreebag <- cforest(y ~ ., data = trainData, controls = bagCtrl)

ctreebag

