# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret", "pROC", "kernlab")
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
# Nonlinear Classification Models:  K-Nearest Neighbors
#   - KNN takes a different approach by using a sample's geographic neighborhood to predict the sample's classification.
#     To allow each predictor to contribute equally to the distance calculation, we recommend centering and scaling all predictors prior to performing KNN.
#   - Class probability estimates for the new sample are calculated as the proportion of training set neighbors in each class.
#     The new sample's predicted class is the class with the highest probability estimate; if two or more classes are tied for the highest estimate,
#     then the tie is broken at random or by looking ahead to eh K+1 closet neighbors.
# ------------------------------------------------------------------------------

set.seed(476)


# Note that we need to set odd number for k
knnGrid = data.frame(k = c(4*(0:5)+1, 20*(1:5)+1, 50*(2:9)+1))


knnFit <- train(x = training[,reducedSet], y = training$Class, method = "knn", metric = "ROC",
                preProc = c("center", "scale"), tuneGrid = knnGrid, trControl = ctrl)


knnFit



# ----------
# There is a distinct jump in predictive performance from 1 to 5 neighbors and a continued steady increase in performance through the range of tuning.
# The initial jump in predictive performance indicates that local geographic information is highly informative for categorizing samples.
# The steady incremental increase in predictive performance furthermore implies that neighborhoods of informative information
# for catgorizing samples are quite large.
# This pattern is somewhat unusual for KNN.

# This example helps to identify a numerical instability problem with KNN: as the number of neighbor increases, the probability of ties also increases.
# For this example, a neighborhood size greater than 451 leads to too many ties.
plot(knnFit)



# ----------
# confusion matrix and other statistics
confusionMatrix(knnFit, norm = "none")



# ----------
# ROC Curve
knnFit$pred <- merge(knnFit$pred,  knnFit$bestTune)

knn_roc <- roc(response = knnFit$pred$obs, predictor = knnFit$pred$successful, levels = rev(levels(knnFit$pred$obs)))


plot(knn_roc, type = "s", legacy.axes = TRUE, col = "blue", lty = 1)


auc(knn_roc)
ci(knn_roc)


# -->
# For these data, the predictive ability of KNN is inferior to the other tuned nonlinear models.
# While geographic information is predictive, it is not as useful as models that seek to find global optimal separating boundaries.


# ----------
# variable importance
plot(varImp(knnFit, scale = FALSE), top=20, scales = list(y = list(cex = .95)))



# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

knn_ho_pred_prob <- predict(knnFit, newdata = training[-pre2008, reducedSet], type ="prob")
knn_ho_pred <- predict(knnFit, newdata = training[-pre2008, reducedSet], type ="raw")

head(knn_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
knn_ho_roc <- pROC::roc(response = training[-pre2008, "Class"], predictor = knn_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(knn_ho_roc, legacy.axes = TRUE, col = "black")

pROC::auc(knn_ho_roc)



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_knn <- data.frame(obs = training[-pre2008, "Class"], knn_ho_pred_prob = knn_ho_pred_prob$successful, knn_ho_pred = knn_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ knn_ho_pred_prob | obs, data = testRes_knn,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

