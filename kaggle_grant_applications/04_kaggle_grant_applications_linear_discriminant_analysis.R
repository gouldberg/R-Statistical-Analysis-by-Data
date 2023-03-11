# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret", "MASS")
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
# Linear Discriminant Analysis
#
#   - The roots of LDA date back to Fisher (1963) and Welch (1939).
#     For the classification problem, Welch took the approach of minimizing the total probability of misclassification, which depends on class probabilities
#     and multivariate distributions of the predictors.
#     Fisher sought to find the linear combination of the predictors such that the between-group variance was maximized relative to the within-group
#     variance.
#   - A linear function defines the separating class boundaries, hence the method's name: LDA.
#     A slight alteration in the assumptions, that the covariance matrices are not identical across the groups, leads to quadractic discriminant analysis.
#   - In general, model requires C * P + P * (P + 1) / 2 parameters with P predictors and C classes.
#     When P = 2 and C = 2, LDA model requires 7 parameters (4 means and 3 variance), while logistic regression only estimate 3 parameters.
#     However, the value of the extra parameters in LDA models is that the between-predictor correlations are explicitly handled by the model.
#     This should provide some advantage to LDA over logistic regression when there are substantial correlations, although both models will break
#     down when the multicollinearity becomes extreme.
#
#   - Practioners should be particularly rigorous in pre-processing data before using LDA. We recommend that predictors be centered and scaled
#     and that near-zero variance predictors be removed. If the covariance matrix is still not invertible, then we recommend using
#     PLS or a regularization approach.
#
#   - As the number of predictors approaches the number of samples, the class probabilities begin to diverge towards the two extremens (0 and 1),
#     indicating that the class probability estimates are poorly calibrate.
#     We recommend that LDA be used on data sets that have at least 5-10 times more samples than predictors. Caution should be applied to LDA results
#     when the ratio dips below 5.
# ------------------------------------------------------------------------------
# Fit the model to the reduced set
# The number of linear discriminants to retain to be tuned automatically.
# Recall that because these data involve two classes, only one discriminant vector can be obtained.

# check the ratio of number of predictors and the number of samples --> more than 10 times, we can model this dataset by LDA.
nrow(training) / length(reducedSet)


set.seed(476)

ldaFit <- train(x = training[, reducedSet], y = training$Class, method = "lda", preProc = c("center","scale"), metric = "ROC", trControl = ctrl)

ldaFit



# ----------
# confusion matrix and other statistics
confusionMatrix(ldaFit, norm = "none")



# ----------
# ROC Curve
ldaFit$pred <- merge(ldaFit$pred,  ldaFit$bestTune)

lda_roc <- roc(response = ldaFit$pred$obs, predictor = ldaFit$pred$successful, levels = rev(levels(ldaFit$pred$obs)))

plot(lr_red_roc, legacy.axes = TRUE, col = "darkgray")
plot(lda_roc, legacy.axes = TRUE, add = TRUE, col = "blue")

auc(lr_red_roc);  auc(lda_roc);
ci(lr_red_roc);  ci(lda_roc);


# -->
# The AUC by LDA is 0.89, and sensitivity 80.88%, specificity = 82.17%
# (specificity is higher than the model by logistic regression)



# ------------------------------------------------------------------------------
# model by MASS::lda and extract coefficients
# ------------------------------------------------------------------------------
# preprocessing
grantPreProcess <- preProcess(training[pre2008, reducedSet])

grantPreProcess

scaledPre2008 <- predict(grantPreProcess, newdata = training[pre2008, reducedSet])
scaled2008HoldOut <- predict(grantPreProcess, newdata = training[-pre2008, reducedSet])



# ----------
ldaModel <- lda(x = scaledPre2008, grouping = training$Class[pre2008])



# ----------
# discriminant values by each class
plot(ldaModel)



# ----------
# Discriminant vector
# This information provides an interpretation about the predictors, relationships among predictors, and, 
# if the data have been centered and scaled, then relative importance values.
head(ldaModel$scaling, 20)
tail(ldaModel$scaling, 20)

rn <- row.names(ldaModel$scaling)
ldaCoef <- data.frame(var = rn, ldacoef = abs(unname(ldaModel$scaling))) %>% arrange(desc(ldacoef))
head(ldaCoef, 20)



# -->
# The Top 5 predictors based on absolute magnitude of discriminant function coefficient
#  1. numeric day of the year (squared):  2.26
#  2. numerci day of the year:-1.07
#  3. ...


# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

lda_ho_pred_prob <- predict(ldaFit, newdata = training[-pre2008, reducedSet], type ="prob")
lda_ho_pred <- predict(ldaFit, newdata = training[-pre2008, reducedSet], type ="raw")

head(lda_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
lda_ho_roc <- roc(response = training[-pre2008, "Class"], predictor = lda_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(lr_red_ho_roc, legacy.axes = TRUE, col = "darkgray")
plot(lda_ho_roc, legacy.axes = TRUE, add = TRUE, col = "blue")

auc(lr_red_ho_roc)
auc(lda_ho_roc)


# -->
# AUC by LDA model for hold-out data:  0.9237



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_lda <- data.frame(obs = training[-pre2008, "Class"], lda_ho_pred_prob = lda_ho_pred_prob$successful, lda_ho_pred = lda_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ lr_red_ho_pred_prob | obs, data = testRes_lr,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

histogram( ~ lda_ho_pred_prob | obs, data = testRes_lda,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")


