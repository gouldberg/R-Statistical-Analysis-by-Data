# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret", "pamr", "pROC")
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
# Nonlinear Classification Models:  Mixture Discriminant Analysis
#
#   - MDA was developed by Hastie and Tibshirani (1996) as an extension of LDA.
#     LDA assumes a distribution of the predictor data such that the class-specific means are different, but the covariance structure is
#     independent of the classes.
#     MDA generalizes LDA in a different manner; it allows each class to be represented by multiple multivariate normal distributions.
#     These distributions can have different means but, like LDA, the covariance structures are assumed to be the same.
#     The modeler would specify how many different distributions should be used and the MDA model would determine their optimal locations in the predictor space.
#   - Similar to LDA, Clemmensen et al. (2011) descrive using ridge- and lasso-like penalties to MDA, which would integrate feature selection into the MDA model.
# ------------------------------------------------------------------------------
set.seed(476)


mdaFit <- train(x = training[,reducedSet], y = training$Class, method = "mda",
                metric = "ROC", tries = 40, tuneGrid = expand.grid(subclasses = 1:8), trControl = ctrl)


mdaFit


plot(mdaFit)



mdaFit$results <- mdaFit$results[!is.na(mdaFit$results$ROC),]                
mdaFit$pred <- merge(mdaFit$pred,  mdaFit$bestTune)
mdaCM <- confusionMatrix(mdaFit, norm = "none")
mdaCM

mdaRoc <- roc(response = mdaFit$pred$obs,
              predictor = mdaFit$pred$successful,
              levels = rev(levels(mdaFit$pred$obs)))
mdaRoc

update(plot(mdaFit,
            ylab = "ROC AUC (2008 Hold-Out Data)"))




# ----------
# List the predictors used at the optimal threshold determined by train.
# 36 predictors were selected.
predictors(nscFit)




# ----------
# The model was tuned over 30 values of the shrinkage parameter, ranging from 0 (implying very little shrinkage and feature selection) to 25.
# When the threshold is lowered to approximately 17, five predictors have been added.
#  - the number of unsuccessful grants by chief investigators, unknown sponsor, contract value band A, unknown contract value band, submission month of Jan.
# The sharp peak at a threshold of 8.6 is curious.
#  - The increase is associated with the removal of 2 predictors: sponsor code 2B and contract value band F.
#  - However, the next shrinkage value removes 3 additional predictors, but this results in a appreciable drop in AUC.

# This spurious jump in performance is likely due to the fact that only a single holdout is used to measure performance.
# The true relationship between performance and shrinkage is likely to be more smooth.
plot(nscFit)



# ----------
# confusion matrix and other statistics
confusionMatrix(nscFit, norm = "none")



# ----------
# ROC Curve
nscFit$pred <- merge(nscFit$pred,  nscFit$bestTune)

nsc_roc <- pROC::roc(response = nscFit$pred$obs, predictor = nscFit$pred$successful, levels = rev(levels(nscFit$pred$obs)))

plot(lr_red_roc, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE)
plot(lda_roc, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE, add = TRUE)
plot(pls_red_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "red")
plot(glmn_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "blue")
plot(spLDA_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "blue")
plot(nsc_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "black")


auc(lda_roc);  auc(pls_red_roc);  auc(glmn_roc);  auc(spLDA_roc);  auc(nsc_roc);
ci(lda_roc);  ci(pls_red_roc);  ci(glmn_roc);  ci(spLDA_roc);  ci(nsc_roc);


# -->
# The AUC by Nearest Shrunken Centroids model (full set) is 0.8753, and sensitivity 86.84%, specificity = 74.27%



# ----------
# variable importance
# based on the distance between the class centroid and the overall centroid
plot(varImp(nscFit, scale = FALSE), top=40, scales = list(y = list(cex = .95)))



# ------------------------------------------------------------------------------
# model by pamr
# ------------------------------------------------------------------------------
# The function to train the model is pamr.train, which takes the input data in a single list object with components x and y.
# pamr.train requires the training set predictors to be encoded in the opposite format where rows are predictors and columns are samples
inputData <- list(x = t(training[pre2008, fullSet]), y = training$Class)



# ----------
# By default, the function chooses 30 appropriate shrinkage values to evaluate,
nscModel <- pamr.train(data = inputData)



# ----------
nscModel



# ----------
# prediction
exampleData <- t(training[1:5, fullSet])

pamr.predict(nscModel, newx = exampleData, threshold = 5)



# ----------
# Which predictors were used at this threshold ? The predict function shows the column numbers for the retained predictors.
thresh17Vars <- pamr.predict(nscModel, newx = exampleData, threshold = 17, type = "nonzero")

fullSet[thresh17Vars]



# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

nsc_ho_pred_prob <- predict(nscFit, newdata = training[-pre2008, fullSet], type ="prob")
nsc_ho_pred <- predict(nscFit, newdata = training[-pre2008, fullSet], type ="raw")

head(nsc_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
nsc_ho_roc <- pROC::roc(response = training[-pre2008, "Class"], predictor = nsc_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(lda_ho_roc, legacy.axes = TRUE, col = "darkgray")
plot(pls_red_ho_roc, legacy.axes = TRUE, add = TRUE, col = "red")
plot(glmn_ho_roc, legacy.axes = TRUE, add = TRUE, col = "blue")
plot(spLDA_ho_roc, legacy.axes = TRUE, add = TRUE, col = "tan")
plot(nsc_ho_roc, legacy.axes = TRUE, add = TRUE, col = "black")

pROC::auc(lda_ho_roc)
pROC::auc(pls_red_ho_roc)
pROC::auc(glmn_ho_roc)
pROC::auc(spLDA_ho_roc)
pROC::auc(nsc_ho_roc)


# -->
# AUC by Nearest Shrunken Centroids model for hold-out data:  0.8652



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_nsc <- data.frame(obs = training[-pre2008, "Class"], nsc_ho_pred_prob = nsc_ho_pred_prob$successful, nsc_ho_pred = nsc_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ lda_ho_pred_prob | obs, data = testRes_lda,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

histogram( ~ glmn_ho_pred_prob | obs, data = testRes_glmn,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

histogram( ~ spLDA_ho_pred_prob | obs, data = testRes_spLDA,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

histogram( ~ nsc_ho_pred_prob | obs, data = testRes_nsc,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

# -->
# The distribution of probability for UNSUCCESSFUL class resulted from sparseLDA model has two modes
# Also the probability for SUCCESSFUL should be larger.
