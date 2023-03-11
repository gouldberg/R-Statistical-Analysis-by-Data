# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret", "glmnet", "pROC")
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
# Penalized Models: glmnet model
#
#   - One might include a penalty term for the logistic regression model in a manner that is very similar to ridge regression.
#     Logistic regression finds parameter values that maximize the binomial likelihood function. A simple approach to regularizing this model
#     would be to add a squared penalty function to the log likelihood.
#     Eilers et al. (2001) and Park and Hastie (2008) discuss this model in the context of data where there are a largenumber of predictors and
#     a small training set sample. In these situations, the penalty term can stabilize the logistic regression model coefficients. As with ridge regression,
#     adding a penalty can also provide a countermeasure against highly correlated predictors.
#
#   - Another method for regularizing linear regression models is to add a penalty based on the absolute values of the regression coefficients
#     (similar to the lasso model). The "glmnet" model (Friedman et al. 2010) uses a lasso-like penalty on the binomial (or multinomial) likelihood function.
#     The glmnet models uses ridge and lasso penalties simultaneously, like the elastic net, but structures the penalty slightly differently.
# ------------------------------------------------------------------------------
glmnGrid <- expand.grid(alpha = c(0, .1, .2, .4, .6, .8, 1), lambda = seq(.01, .3, length = 80))

set.seed(476)


# Fit by full set data
glmnFit <- train(x = training[,fullSet], y = training$Class, method = "glmnet",
                 tuneGrid = glmnGrid, preProc = c("center", "scale"), metric = "ROC", trControl = ctrl)


glmnFit


# ----------
# confusion matrix and other statistics
confusionMatrix(glmnFit, norm = "none")



# ----------
# ROC Curve:  NOTE that pROC will be masked by glmnet, so we need pROC::roc
glmnFit$pred <- merge(glmnFit$pred,  glmnFit$bestTune)

glmn_roc <- pROC::roc(response = glmnFit$pred$obs, predictor = glmnFit$pred$successful, levels = rev(levels(glmnFit$pred$obs)))

plot(lr_red_roc, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE)
plot(lda_roc, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE, add = TRUE)
plot(pls_red_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "red")
plot(glmn_roc, type = "s", add = TRUE, legacy.axes = TRUE, col = "blue")

auc(lda_roc);  auc(pls_red_roc);  auc(glmn_roc);
ci(lda_roc);  ci(pls_red_roc);  ci(glmn_roc);


# -->
# The AUC by glmnet (full set) is 0.9093, and sensitivity 89.12%, specificity = 78.32%



# ----------
# Heatmap
# Hyper parameter tuning:  ROC metric by Mixing Percentage (alpha) * Regularization Parameter (lamda)
# alpha = 0.1 and lambd = 0.1972 results highest ROC, indicating that this data set favor models witha a larger mix of the ridge penalty than the lasso penalty,
# although there are many choices in this grid that are comparable.

glmnFit0 <- glmnFit
glmnFit0$results$lambda <- format(round(glmnFit0$results$lambda, 3))
plot(glmnFit0, plotType = "level", cuts = 15, scales = list(x = list(rot = 90, cex = .65)))

glmnPlot <- plot(glmnFit0, plotType = "level", cuts = 15, scales = list(x = list(rot = 90, cex = .65)))
update(glmnPlot,
       ylab = "Mixing Percentage\nRidge <---------> Lasso",
       sub = "",
       main = "Area Under the ROC Curve",
       xlab = "Amount of Regularization")


# OR this plot
plot(glmnFit)



# ----------
# variable importance
plot(varImp(glmnFit, scale = FALSE), top=20, scales = list(y = list(cex = .95)))



# ------------------------------------------------------------------------------
# model by glmnet function
# ------------------------------------------------------------------------------
# glmnet function takes
#  x:  matrix of predictors   y:  factor of classes (for logistic regression)
#  family = "binomial" corresponds to logistic regression, and when there are three or more classes, family = "multinomial" is appropriate.
# The function will automatically select a sequence of values for the amount of regularization, although the user can select their own values
# with the lambda option.
# glmnet defaults mixing parameter (alpha) = 1, corresponding to a complete lasso penalty.
glmnetModel <- glmnet(x = as.matrix(training[pre2008, fullSet]), y = training[pre2008, "Class"], family = "binomial")



# ----------
glmnetModel

plot(glmnetModel)



# ----------
# Compute predictions for 3 different types of values
predict(glmnetModel, newx = as.matrix(training[1:5, fullSet]), s = c(0.05, 0.1, 0.2), type = "class")



# ----------
# Which predictors were used in the model ?
predict(glmnetModel, newx = as.matrix(training[1:5, fullSet]), s = c(0.05, 0.1, 0.2), type = "nonzero")



# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

glmn_ho_pred_prob <- predict(glmnFit, newdata = training[-pre2008, fullSet], type ="prob")
glmn_ho_pred <- predict(glmnFit, newdata = training[-pre2008, fullSet], type ="raw")

head(glmn_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
glmn_ho_roc <- pROC::roc(response = training[-pre2008, "Class"], predictor = glmn_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(lda_ho_roc, legacy.axes = TRUE, col = "darkgray")
plot(pls_red_ho_roc, legacy.axes = TRUE, add = TRUE, col = "red")
plot(glmn_ho_roc, legacy.axes = TRUE, add = TRUE, col = "blue")

pROC::auc(lda_ho_roc)
pROC::auc(pls_red_ho_roc)
pROC::auc(glmn_ho_roc)


# -->
# AUC by glmnet (penalized regression) model for hold-out data:  0.9156



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_glmn <- data.frame(obs = training[-pre2008, "Class"], glmn_ho_pred_prob = glmn_ho_pred_prob$successful, glmn_ho_pred = glmn_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ lda_ho_pred_prob | obs, data = testRes_lda,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

histogram( ~ glmn_ho_pred_prob | obs, data = testRes_glmn,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")


# -->
# The distribution of probability resulted from glmnet:  poorly calibrated

