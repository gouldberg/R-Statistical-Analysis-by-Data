# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret", "sparseLDA", "pROC")
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
# Penalized LDA model
#
#   - Clemmensen et al. (2011) use penalization strategy with LDA models using the flexible discriminant analysis (FDA) framework.
#     In this model, an elastic-net strategy is used; L1 penalties have the effect of eliminating predictors while an L2 penalty shrinks the coefficients
#     of the discriminant functions towards 0.
# ------------------------------------------------------------------------------
set.seed(476)

spldaGrid <- expand.grid(lambda = c(.1), NumVars = c(1:20, 50, 75, 100, 250, 500, 750, 1000))

spLDAFit <- train(x = training[,fullSet], y = training$Class, method = "sparseLDA",
                  tuneGrid = spldaGrid, preProc = c("center", "scale"), metric = "ROC", trControl = ctrl)


spLDAFit


# As the penalty increases and predictors are eliminated performance improves and remains relatively constant until important factors are removed.
# As a result of the tuning process, six predictors were used in the model
plot(spLDAFit)



# ----------
# confusion matrix and other statistics
confusionMatrix(spLDAFit, norm = "none")



# ----------
# ROC Curve
spLDAFit$pred <- merge(spLDAFit$pred,  spLDAFit$bestTune)

spLDA_roc <- pROC::roc(response = spLDAFit$pred$obs, predictor = spLDAFit$pred$successful, levels = rev(levels(spLDAFit$pred$obs)))

plot(lr_red_roc, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE)
plot(lda_roc, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE, add = TRUE)
plot(pls_red_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "red")
plot(glmn_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "blue")
plot(spLDA_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "blue")

auc(lda_roc);  auc(pls_red_roc);  auc(glmn_roc);  auc(spLDA_roc);
ci(lda_roc);  ci(pls_red_roc);  ci(glmn_roc);  ci(spLDA_roc);


# -->
# The AUC by sparseLDA model (full set) is 0.9015, and sensitivity 92.11%, specificity = 71.94%



# ----------
# variable importance
plot(varImp(spLDAFit, scale = FALSE), top=20, scales = list(y = list(cex = .95)))



# ------------------------------------------------------------------------------
# model by sparseLDA:::sda function
# ------------------------------------------------------------------------------
# sparseLDA:::sda function has an argument for the ridge parameter called "lambda".
# The lasso penalty can be stated in two possible ways with the argument "stop".
#   - The magnitude of the lasso penalty is controlled using a positive number (e.g., stop = 0.01) or,
#   - alternatively, the number of retained predictors can be chosen using a negative integer (e.g., stop = -6 for 6 predictors)
sparseLdaModel <- sparseLDA:::sda(x = as.matrix(training[pre2008, fullSet]), y = training[pre2008, "Class"], lambda = 0.01, stop = -6)



# ----------
sparseLdaModel



# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

spLDA_ho_pred_prob <- predict(spLDAFit, newdata = training[-pre2008, fullSet], type ="prob")
spLDA_ho_pred <- predict(spLDAFit, newdata = training[-pre2008, fullSet], type ="raw")

head(spLDA_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
spLDA_ho_roc <- pROC::roc(response = training[-pre2008, "Class"], predictor = spLDA_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(lda_ho_roc, legacy.axes = TRUE, col = "darkgray")
plot(pls_red_ho_roc, legacy.axes = TRUE, add = TRUE, col = "red")
plot(glmn_ho_roc, legacy.axes = TRUE, add = TRUE, col = "blue")
plot(spLDA_ho_roc, legacy.axes = TRUE, add = TRUE, col = "tan")

pROC::auc(lda_ho_roc)
pROC::auc(pls_red_ho_roc)
pROC::auc(glmn_ho_roc)
pROC::auc(spLDA_ho_roc)


# -->
# AUC by sparseLDA model for hold-out data:  0.9011



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_spLDA <- data.frame(obs = training[-pre2008, "Class"], spLDA_ho_pred_prob = spLDA_ho_pred_prob$successful, spLDA_ho_pred = spLDA_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ lda_ho_pred_prob | obs, data = testRes_lda,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

histogram( ~ glmn_ho_pred_prob | obs, data = testRes_glmn,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

histogram( ~ spLDA_ho_pred_prob | obs, data = testRes_spLDA,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

# -->
# The distribution of probability for UNSUCCESSFUL class resulted from sparseLDA model has two modes

