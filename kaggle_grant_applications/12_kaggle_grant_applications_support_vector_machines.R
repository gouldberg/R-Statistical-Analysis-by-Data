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
# Nonlinear Classification Models:  Support Vector Machines
#   - For support vector machines, cost values are used to penalize number of errors; as a consequence, larger cost values induce higher model
#     complexity rather than restrain it.
#   - Due to the dot product (kernel trick), the predicotr data should be centered and scaled prior to fitting so that attributes whose values are 
#     large in magnitude do not dominate the calculations.
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(cores = 20)


# ----------
# full set + svmRadial
set.seed(201)
sigmaRangeFull <- sigest(as.matrix(training[,fullSet]))
svmRGridFull <- expand.grid(sigma =  as.vector(sigmaRangeFull)[1], C = 2^(-3:4))

set.seed(476)

svmRFitFull <- train(x = training[,fullSet], y = training$Class, method = "svmRadial", metric = "ROC",
                     preProc = c("center", "scale"), tuneGrid = svmRGridFull, trControl = ctrl)

svmRFitFull

plot(svmRFitFull)



# ----------
# reduced set + svmRadial

set.seed(202)
sigmaRangeReduced <- sigest(as.matrix(training[,reducedSet]))
svmRGridReduced <- expand.grid(sigma = sigmaRangeReduced[1], C = 2^(seq(-4, 4)))

set.seed(476)

svmRFitReduced <- train(x = training[,reducedSet], y = training$Class, method = "svmRadial", metric = "ROC",
                        preProc = c("center", "scale"), tuneGrid = svmRGridReduced, trControl = ctrl)

svmRFitReduced

plot(svmRFitReduced)



# ----------
# full set + svmPoly
svmPGrid <-  expand.grid(degree = 1:2, scale = c(0.01, .005), C = 2^(seq(-6, -2, length = 10)))

set.seed(476)

svmPFitFull <- train(x = training[,fullSet], y = training$Class, method = "svmPoly", metric = "ROC", preProc = c("center", "scale"),
                     tuneGrid = svmPGrid, trControl = ctrl)

svmPFitFull

plot(svmPFitFull)



# ----------
# reduced set + svmPoly
svmPGrid2 <-  expand.grid(degree = 1:2, scale = c(0.01, .005), C = 2^(seq(-6, -2, length = 10)))

set.seed(476)

svmPFitReduced <- train(x = training[,reducedSet], y = training$Class, method = "svmPoly", metric = "ROC",
                        preProc = c("center", "scale"), tuneGrid = svmPGrid2, fit = FALSE, trControl = ctrl)

svmPFitReduced

plot(svmPFitReduced)



# ----------
# confusion matrix and other statistics
confusionMatrix(svmRFitFull, norm = "none")
confusionMatrix(svmRFitReduced, norm = "none")
confusionMatrix(svmPFitFull, norm = "none")
confusionMatrix(svmPFitReduced, norm = "none")



# ----------
# ROC Curve
svmRFitFull$pred <- merge(svmRFitFull$pred,  svmRFitFull$bestTune)
svmRFitReduced$pred <- merge(svmRFitReduced$pred,  svmRFitReduced$bestTune)
svmPFitFull$pred <- merge(svmPFitFull$pred,  svmPFitFull$bestTune)
svmPFitReduced$pred <- merge(svmPFitReduced$pred,  svmPFitReduced$bestTune)


svmRFull_roc <- roc(response = svmRFitFull$pred$obs, predictor = svmRFitFull$pred$successful, levels = rev(levels(svmRFitFull$pred$obs)))
svmRReduced_roc <- roc(response = svmRFitReduced$pred$obs, predictor = svmRFitReduced$pred$successful, levels = rev(levels(svmRFitReduced$pred$obs)))
svmPFull_roc <- roc(response = svmPFitFull$pred$obs, predictor = svmPFitFull$pred$successful, levels = rev(levels(svmPFitFull$pred$obs)))
svmPReduced_roc <- roc(response = svmPFitReduced$pred$obs, predictor = svmPFitReduced$pred$successful, levels = rev(levels(svmPFitReduced$pred$obs)))


plot(fda_roc, type = "s", legacy.axes = TRUE, col = "darkgray")
plot(svmRFull_roc, type = "s", legacy.axes = TRUE, add = TRUE, col = "red", lty = 3)
plot(svmRReduced_roc, type = "s", legacy.axes = TRUE, add = TRUE, col = "blue", lty = 3)
plot(svmPFull_roc, type = "s", legacy.axes = TRUE, add = TRUE, col = "red", lty = 1)
plot(svmPReduced_roc, type = "s", legacy.axes = TRUE, add = TRUE, col = "blue", lty = 1)


auc(fda_roc);  auc(svmRFull_roc);  auc(svmRReduced_roc);  auc(svmPFull_roc);  auc(svmPReduced_roc);
ci(fda_roc);  ci(svmRFull_roc);  ci(svmRReduced_roc);  ci(svmPFull_roc);  ci(svmPReduced_roc);



# -->
# reducedSet predictor yields better results than fullSet, with a noptimal AUC is 0.895, with polynomical degree = 2, AUC is 0.897


# ----------
svmRadialResults <- rbind(svmRFitReduced$results, svmRFitFull$results)
svmRadialResults$Set <- c(rep("Reduced Set", nrow(svmRFitReduced$result)), rep("Full Set", nrow(svmRFitFull$result)))
svmRadialResults$Sigma <- paste("sigma = ", format(svmRadialResults$sigma,  scientific = FALSE, digits= 5))
svmRadialResults <- svmRadialResults[!is.na(svmRadialResults$ROC),]

xyplot(ROC ~ C | Set, data = svmRadialResults, groups = Sigma, type = c("g", "o"), 
       xlab = "Cost", ylab = "ROC (2008 Hold-Out Data)", auto.key = list(columns = 2), scales = list(x = list(log = 2)))


svmPolyResults <- rbind(svmPFitReduced$results, svmPFitFull$results)
svmPolyResults$Set <- c(rep("Reduced Set", nrow(svmPFitReduced$result)), rep("Full Set", nrow(svmPFitFull$result)))
svmPolyResults <- svmPolyResults[!is.na(svmPolyResults$ROC),]
svmPolyResults$scale <- paste("scale = ", format(svmPolyResults$scale, scientific = FALSE))
svmPolyResults$Degree <- "Linear"
svmPolyResults$Degree[svmPolyResults$degree == 2] <- "Quadratic"

useOuterStrips(xyplot(ROC ~ C | Degree*Set, data = svmPolyResults, groups = scale, type = c("g", "o"),
                      xlab = "Cost", ylab = "ROC (2008 Hold-Out Data)", auto.key = list(columns = 2), scales = list(x = list(log = 2))))


# -->
# Optimal performane for linear and quadratic models was about the same.
# This suggests that the models are mostly picking up on linear relationships in the data.
# Given that many of the predictors are binary, this makes sense.

# Of these models, the best AUC was 0.897.



# ----------
# variable importance
plot(varImp(svmRFitReduced, scale = FALSE), top=20, scales = list(y = list(cex = .95)))



# ------------------------------------------------------------------------------
# model by kernlab package
#   - Although the epsilon paramter is only relevant for regression, a few other parameters are useful for classification.
#   - The logical prob.model argument triggers ksvm to estimate an additional set of parameters for a sigmoidal function to translate the SVM decision values
#     to class probabilities using the method of Platt (2000). If this option is not set to TRUE, class probabilities can not be predicted.
#   - The class.weights argument assigns asymmetric costs to each class (Osuna et al. 1997). This can be especially important when one or more specific
#     types of errors are more harmful than others or when there is a severe class imbalance that biases the model to the majority class.
#     The syntax here is to use a named vector of weights or costs.
#     For example, if there was a desire to bias the grant model to detect unsuccessful grants, then the syntax would be:
#     class.weights = c(successfule = 1, unsuccessful = 5)
#     This makes a false-negative error five times more costly than a false-positive error.
#     Note that the implementation of class weights in ksvm affects the predicted class, but the class probability model is affected by the weights.
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

svm_ho_pred_prob <- predict(svmPFitReduced, newdata = training[-pre2008, reducedSet], type ="prob")
svm_ho_pred <- predict(svmPFitReduced, newdata = training[-pre2008, reducedSet], type ="raw")

head(svm_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
svm_ho_roc <- pROC::roc(response = training[-pre2008, "Class"], predictor = svm_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(fda_ho_roc, legacy.axes = TRUE, col = "darkgray")
plot(svm_ho_roc, legacy.axes = TRUE, col = "black", add = TRUE)

pROC::auc(svm_ho_roc)



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_svm <- data.frame(obs = training[-pre2008, "Class"], svm_ho_pred_prob = svm_ho_pred_prob$successful, svm_ho_pred = svm_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ svm_ho_pred_prob | obs, data = testRes_svm,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

