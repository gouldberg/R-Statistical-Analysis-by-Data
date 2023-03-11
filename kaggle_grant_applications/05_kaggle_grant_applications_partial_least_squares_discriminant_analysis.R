# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret")
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
# Partial Least Squares Discriminant Analysis
#
#   - Just like in the regression setting, we can attempt to pre-process data in a way that removes highly correlated predictors.
#     If more complex correlation structure exist in the data or if the number of predictors still exceeds the number of samples,
#     then we recommend using PLS for the purpose of discrimiation.
#   - The application of PLS to a classification problem dates back to at leat the mid 1980s (Berntsson and Wold 1986). The original NIPALS
#     algorithm was developed and refined in the chemometrics community. Not surprisingly, this community explored and extended the use of PLS
#     to the classification setting and termed this technique PLS discriminant analysis (or PLSDA).
#     Dunn and Wold (1990), for example, illustrated PLSDA on a chemometrics pattern recognition example and showed that it provided a better
#     separation of the samples into groups than the traditional PCA-then-LDA approach.
#
#   - Liu and Rayens (2007) point out that if dimension reduction is NOT necessary and classification is the goal,
#     then LDA will ALWAYS provide a lower misclassification rate than PLS. Hence, LDA still has a necessary place in the classification toolbox.
# ------------------------------------------------------------------------------
# Fit the model to the reduced set and full set
# Use Bayes method to compute the probabilities
set.seed(476)

plsFit_full <- train(x = training[,fullSet], y = training$Class, method = "pls", tuneGrid = expand.grid(ncomp = 1:10),
                preProc = c("center","scale"), metric = "ROC", probMethod = "Bayes", trControl = ctrl)

set.seed(476)
plsFit_red <- train(x = training[,reducedSet], y = training$Class, method = "pls", tuneGrid = expand.grid(ncomp = 1:10),
                     preProc = c("center","scale"), metric = "ROC", probMethod = "Bayes", trControl = ctrl)


plsFit_full
plsFit_red


plot(plsFit_full)

plot(plsFit_red)



# ----------
# confusion matrix and other statistics
confusionMatrix(plsFit_full, norm = "none")
confusionMatrix(plsFit_red, norm = "none")



# ----------
# ROC Curve
plsFit_full$pred <- merge(plsFit_full$pred,  plsFit_full$bestTune)
plsFit_red$pred <- merge(plsFit_red$pred,  plsFit_red$bestTune)

pls_full_roc <- roc(response = plsFit_full$pred$obs, predictor = plsFit_full$pred$successful, levels = rev(levels(plsFit_full$pred$obs)))
pls_red_roc <- roc(response = plsFit_red$pred$obs, predictor = plsFit_red$pred$successful, levels = rev(levels(plsFit_red$pred$obs)))

plot(lr_red_roc, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE)
plot(lda_roc, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE, add = TRUE)
plot(pls_full_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "blue")
plot(pls_red_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "red")

auc(lda_roc);  auc(pls_full_roc);  auc(pls_red_roc);
ci(lda_roc);  ci(pls_full_roc);  ci(pls_red_roc);


# -->
# The AUC by PLS (reduced set) is 0.8917, and sensitivity 83.33%, specificity = 79.33%
# AUC by PLS (reduced set) is almost same level with LDA model (slightly bettern than LDA)

# Including predictors that contain very little or no information about the response degrades the performance of a PLS model.
# The smaller set of predictors improves the ROC, uses fewer components to get to that value, and attains a value that is equivalent to the LDA
# model's performance.



# ----------
# variable importance
plot(varImp(plsFit_full, scale = FALSE), top=20, scales = list(y = list(cex = .95)))



# ------------------------------------------------------------------------------
# model by caret::plsda
#   - caret package contains a function (plsda) that can create the appropriate dummy variable PLS model for the data and then post-process the raw model predictions
#     to return class probabilities
# ------------------------------------------------------------------------------
# Use Bayes method to compute the probabilities (not "softmax")
plsdaModel <- caret::plsda(x = training[pre2008, reducedSet], y = training[pre2008, "Class"], scale = TRUE, 
                         probMethod = "Bayes", ncomp = 6)


# ----------
plot(plsdaModel)


scoreplot(plsdaModel)


loadings(plsdaModel)



# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

pls_red_ho_pred_prob <- predict(plsFit_red, newdata = training[-pre2008, reducedSet], type ="prob")
pls_red_ho_pred <- predict(plsFit_red, newdata = training[-pre2008, reducedSet], type ="raw")

head(pls_red_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
pls_red_ho_roc <- roc(response = training[-pre2008, "Class"], predictor = pls_red_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(lda_ho_roc, legacy.axes = TRUE, col = "darkgray")
plot(pls_red_ho_roc, legacy.axes = TRUE, add = TRUE, col = "blue")

auc(lda_ho_roc)
auc(pls_red_ho_roc)


# -->
# AUC by PLSDA model for hold-out data:  0.9215,  almost equivalent to LDA model



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_pls <- data.frame(obs = training[-pre2008, "Class"], pls_red_ho_pred_prob = pls_red_ho_pred_prob$successful, pls_red_ho_pred = pls_red_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ lda_ho_pred_prob | obs, data = testRes_lda,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

histogram( ~ pls_red_ho_pred_prob | obs, data = testRes_pls,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")


# -->
# The distribution of probability resulted from PLSDA model is more diverged than that of LDA model

