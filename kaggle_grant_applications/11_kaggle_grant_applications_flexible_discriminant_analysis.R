# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret", "pROC", "mda", "earth")
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
# Nonlinear Classification Models:  Flexible Discriminant Analysis
#   - Hastie et al. (1994) describe a process where, for C classes, a set of C linear regression models can be fit to binary class indicators and show
#     that the regression coefficients from these models can be post-processed to derive the discriminant coefficients.
#     This allows the idea of linear discriminant analysis to be extended in a number of ways.
#     First, many of the models, such as lasso, ridge regression, or MARS, can be extended to create discriminant variables.
#     For example, MARS can be used to create a set of hinge functions that result in discriminant functions that are nonlinear combinations of the original predictors.
#     This conceptual framework is referred to as flexible discriminant analysis (FDA).
#     Essentially, the MARS features isolate multidimensional polytopal regions of the predictor space and predict a common class within these regions.
#   - Bagging the model coerces FDA to produce smoother relationships between the predictors and the outcome. MARS models are moderately unstable predictors
#     since they use exhaustive searches of the data and the splits are based on specific data points in the training set.
#     Bagging the FDA model will have the effect of adding more splits for the important predictors, leading to a better approximation.
#     However, our experience is that bagging MARS or FDA models has a marginal impact on model performance and increased number of terms diminishes the 
#     interpretation of the discriminant equation.
#   - Since many of the predictors in the FDA model are on different scales, it is difficult to use the discriminant function to uncover which variables have
#     the most impact on the outcome.
# ------------------------------------------------------------------------------

set.seed(476)

fdaFit <- train(x = training[,reducedSet], y = training$Class, method = "fda", metric = "ROC",
                tuneGrid = expand.grid(degree = 1, nprune = 2:25), trControl = ctrl)

fdaFit



# ----------
# First-degree MARS hinge functions were evaluated where the number of retained terms ranged from 2 to 25.
# Performance increases as the number of terms increases and plateaus around 15 terms.
# The numerically optimal value was 19 although there is clearly some flexibility in thie parameter.
update(plot(fdaFit), ylab = "ROC AUC (2008 Hold-Out Data)")


# Although the FDA model contained 19 terms, 14 unique predictors were used (of a possible 1,070)

# ----------
# confusion matrix and other statistics
confusionMatrix(fdaFit, norm = "none")



# ----------
# ROC Curve
fdaFit$pred <- merge(fdaFit$pred,  fdaFit$bestTune)

fda_roc <- pROC::roc(response = fdaFit$pred$obs, predictor = fdaFit$pred$successful, levels = rev(levels(fdaFit$pred$obs)))

plot(nnet4_roc, type = "s", legacy.axes = TRUE, col = "darkgray")
plot(fda_roc, type = "s", legacy.axes = TRUE, add = TRUE, col = "black")


auc(nnet4_roc);  auc(fda_roc);
ci(nnet4_roc);  ci(fda_roc);



# ----------
# variable importance
plot(varImp(fdaFit, scale = FALSE), top=20, scales = list(y = list(cex = .95)))



# ------------------------------------------------------------------------------
# model by fda
#  - To use FDA with MARS, there are 2 approaches. method = mars uses the MARS implementaion in the mda package.
#    However, the earth package fits the MARS model with a wider range of options. Here load the earth package and then specify method = earth.
#  - Note that fda accepts only formula syntax.
# ------------------------------------------------------------------------------
set.seed(800)

tmp = training[pre2008, c(reducedSet, "Class")]

fdaMod <- mda::fda(Class ~ ., data = tmp, method = earth)

fdaMod


# ----------
# Note that the model coefficients here have not been post-processed
summary(fdaMod$fit)



# ----------
# final model coefficients can be found with coef(fdaModel)
coef(fdaMod)



# ----------
predict(fdaMod, newdata = head(training[-pre2008, reducedSet]))




# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

fda_ho_pred_prob <- predict(fdaFit, newdata = training[-pre2008, reducedSet], type ="prob")
fda_ho_pred <- predict(fdaFit, newdata = training[-pre2008, reducedSet], type ="raw")

head(fda_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
fda_ho_roc <- pROC::roc(response = training[-pre2008, "Class"], predictor = fda_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(nnet_ho_roc, legacy.axes = TRUE, col = "darkgray")
plot(fda_ho_roc, legacy.axes = TRUE, col = "black", add = TRUE)

pROC::auc(fda_ho_roc)


# -->
# AUC by Flexible Distriminant Analysis model for hold-out data:  0.9296



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_fda <- data.frame(obs = training[-pre2008, "Class"], fda_ho_pred_prob = fda_ho_pred_prob$successful, fda_ho_pred = fda_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ fda_ho_pred_prob | obs, data = testRes_fda,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

