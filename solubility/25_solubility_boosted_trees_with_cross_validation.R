setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "MASS", "gbm")
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
# Regression Trees and Rule-Based Models:  Boosted Trees
#   - Several researchers connected the AdaBoost algorithm to statistical concepts of loss functions, additive modeling, and logistic regression
#     and showed that boosting can be interpreted as a forward stagewise additive model that minimizes exponential loss.
#     Friedman (2001) developed the "gradient boosting machines" method. The basic principles of this is:
#     given a loss function and a weak learner, the algorithm seeks to find an additive model that minimizes the loss function.
#     The algorithm is typically initialized with the best guess of the response. The gradient (residual) is calculated, and a model is then fit
#     to the residuals to minimize the loss function.
#     The current model is added to the previous model, and the procedure continues for a user-specified number of iterations.
#
#
#   - Because boosting requires a weak learner, almost any technique with tuning parameters can be made into a weka learner. Trees make an
#     excellent base learner for boosting for several reasons.
#        - trees have the flexibility to be wek learners by simply restricting their depth.
#        - separate trees can be easily added together, much like individual predictors can be added together in a regression model, to generate a prediction.
#        - trees can be generated very quickly
#
#     -------------------------
#     Simple Gradient Boosting for Regression  (please note additional improvements described below, such as shrinkage)
#     Select tree depth, D, and number of interactions, K
#     Compute the average response, y-hat, and use this as the initial predicted value for each sample
#     for k = 1 to K do
#         Compute the residual, the difference between the observed value and the "current" predicted value, for each sample
#         Fit a regression tree of depth, D, using the residuals as the response
#         Predict each sample using the regression tree fit in the previous step
#         Update the predicted value of each sample by adding the previous iteration's predicted value to the predicted value generated in the previous step
#     end
#     -------------------------
#
#  - In random forests, all trees are created independently, each tree is created to have maximum depth, 
#    and each tree contributes equally to the final model. The trees in boosting, however, are dependent on past trees, have minimum depth,
#    and contribute unequally to the final model.
#
#  - Boosting select the optimal learner at each stage of the algorithm. Despite using weak learners, boosting still employs the greedy
#    strategy of choosing the optimal weak learner at each stage. This has the drawbacks of not finding the optimal global model
#    as well as over-fitting the training data. A remedy for greediness is to constrain the learning process by employing regularization,
#    or shrinkage.
#    Instead of adding the predicted value for a sample to previous iteration's predicted value, only a fraction of the current predicted value is
#    added to the previous iteration's predicted value.
#    This fraction is commonly referred to as the learning rate and is parameterized by the symbol, lambda.
#    Small values of learning paramter (< 0.01) work best, but the valu of the parameter is inversely proportional to the computation time
#    required to fin an optimal model.
#
#  - Friedman updated the boosting machine algorithm with a random sampling scheme and termed the new procedure "stochastic gradient boosting"
#    Friedman inserted the step of randomly selecting a fraction of the training data within the loop.
#    The residuals and models in the remaining steps of the current iteration are based only on the sample of data.
#    The fraction of training data used, known as the "bagging fraction".
#    It turns out that this simple modification improved the prediction accuracy while also reducing the required computational resources.
#    Friedman suggests using a baggin fraction of around 0.5.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Tune Boosted Trees model by cross-validation
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(10)


# n.trees:  the number of trees
# interaction.depth:  depth of trees
# shrinkage:  
# n.minobsinnode:  
# bag.fraction: proportion of obeservations to be sampled
gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2), n.trees = seq(100, 1000, by = 50), 
                       shrinkage = c(0.01, 0.1), n.minobsinnode = 1)



set.seed(100)

gbmTune <- train(x = solTrainXtrans, y = solTrainY, method = "gbm",
                 tuneGrid = gbmGrid, trControl = ctrl, verbose = TRUE)

gbmTune

gbmTune$bestTune



# ----------
# The bagging fraction is fixed at 0.5.
# The larger value of shrinkage has an impact on reducing RMSE for all choices of tree depth and number of trees.
# Also, RMSE decreases as tree depth increases when shrinkage is 0.01.
# The same pattern holds true for RMSE when shrinkage is 0.1 and the number of trees is less than 300.

# Using the one-standard-error rule, the optimal boosted tree has depth 3 with 400 trees and shrinkage of 0.1,
# producing a cross-validated RMSE of 0.616.
plot(gbmTune, auto.key = list(columns = 4, lines = TRUE))



# ----------
# The importance profile for boosting has a much steeper importance slope than the one for random forests.
# This is due to the fact that the trees from boosting are dependent on each other and hence will have correlated structures as the method follows
# by the gradient. Therefore many of the same predictors will be selected across the trees, increasing their contribution
# to the importance metric.
gbmImp <- varImp(gbmTune, scale = FALSE)

gbmImp

plot(gbmImp, top = 40)




# ------------------------------------------------------------------------------
# LEARNING RATE and BAGGING FRACTION:  Understand how bagging fraction and learning rate affect magnitudes of variable importance
#
#  - Ridgeway (2007) suggests that small values of the learning parameter (< 0.01) work best.
#  - Friedman suggests using bagging fraction (the fraction of training data use) of around 0.5
#
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(10)


# ----------
# learning rate (shrinkage) = 0.1 and find optimal parameters  (bag.fraction here cannot be tuned by caret::train)
gbmGrid_01_tmp <- expand.grid(interaction.depth = seq(1, 7, by = 2), n.trees = seq(100, 1000, by = 50), 
                       shrinkage = 0.1, n.minobsinnode = 1)

set.seed(100)

gbmTune_01_tmp <- train(x = solTrainXtrans, y = solTrainY, method = "gbm",
                 tuneGrid = gbmGrid_01_tmp, trControl = ctrl, verbose = TRUE)

gbmTune_01_tmp$bestTune


# apply optimal parameters with bag.fraction = 0.1
gbmModel_01 <- gbm.fit(solTrainXtrans, solTrainY, distribution = "gaussian", bag.fraction = 0.1,
                       n.trees = 600, interaction.depth = 7, shrinkage = 0.1, n.minobsinnode = 1)


# variable importance
graphics.off();  par(mfrow=c(1,1));
summary(gbmModel_01)

# I can not change the order to show the variable in descending order by its values
gbmModel_01_imp <- data.frame(var = summary(gbmModel_01)$var, imp = summary(gbmModel_01)$rel.inf)
gbmModel_01_imp$id <- nrow(gbmModel_01_imp):1
# rapply(gbmModel_01_imp, as.character, classes = "is.factor", how = "replace")
graphics.off();  par(mfrow=c(1,1));
dotplot(var ~ imp, data = gbmModel_01_imp[1:20,], horizontal = TRUE)
dotplot(id ~ imp, data = gbmModel_01_imp[1:20,], horizontal = TRUE)



# ----------
# learning rate (shrinkage) = 0.9 and find optimal parameters  (bag.fraction here cannot be tuned by caret::train)
gbmGrid_09_tmp <- expand.grid(interaction.depth = seq(1, 7, by = 2), n.trees = seq(100, 1000, by = 50), 
                              shrinkage = 0.9, n.minobsinnode = 1)

set.seed(100)

gbmTune_09_tmp <- train(x = solTrainXtrans, y = solTrainY, method = "gbm",
                        tuneGrid = gbmGrid_09_tmp, trControl = ctrl, verbose = TRUE)

gbmTune_09_tmp$bestTune


# apply optimal parameters with bag.fraction = 0.9
# --> the variable importance decrease steeply more than the case of shrinkage and bag.fraction = 0.1
gbmModel_09 <- gbm.fit(solTrainXtrans, solTrainY, distribution = "gaussian", bag.fraction = 0.9,
                       n.trees = 200, interaction.depth = 1, shrinkage = 0.9, n.minobsinnode = 1)


gbmModel_09_imp <- data.frame(var = summary(gbmModel_09)$var, imp = summary(gbmModel_09)$rel.inf)
gbmModel_09_imp$id <- nrow(gbmModel_09_imp):1
graphics.off();  par(mfrow=c(1,1));
dotplot(var ~ imp, data = gbmModel_09_imp[1:20,], horizontal = TRUE, xlim = c(0,30))
dotplot(id ~ imp, data = gbmModel_09_imp[1:20,], horizontal = TRUE, xlim = c(0,30))



# ------------------------------------------------------------------------------
# DEPTH INTERACTION:  Understand how interaction depth affect affect magnitudes of variable importance
# ------------------------------------------------------------------------------
# learning rate (shrinkage) = 0.1 
# bag.fraction = 0.5
# n.trees = 500 and n.minobsinnode = 1
# change interaction depth 1,3,5,7, and 9
gbmModel_id_1 <- gbm.fit(solTrainXtrans, solTrainY, distribution = "gaussian", bag.fraction = 0.5,
                         n.trees = 500, interaction.depth = 1, shrinkage = 0.1, n.minobsinnode = 1)

gbmModel_id_5 <- gbm.fit(solTrainXtrans, solTrainY, distribution = "gaussian", bag.fraction = 0.5,
                         n.trees = 500, interaction.depth = 5, shrinkage = 0.1, n.minobsinnode = 1)

gbmModel_id_9 <- gbm.fit(solTrainXtrans, solTrainY, distribution = "gaussian", bag.fraction = 0.5,
                         n.trees = 500, interaction.depth = 9, shrinkage = 0.1, n.minobsinnode = 1)

gbmModel_id_15 <- gbm.fit(solTrainXtrans, solTrainY, distribution = "gaussian", bag.fraction = 0.5,
                         n.trees = 500, interaction.depth = 15, shrinkage = 0.1, n.minobsinnode = 1)

gbmModel_id_1_imp <- data.frame(var = summary(gbmModel_id_1)$var, imp = summary(gbmModel_id_1)$rel.inf)
gbmModel_id_1_imp$id <- nrow(gbmModel_id_1_imp):1

gbmModel_id_5_imp <- data.frame(var = summary(gbmModel_id_5)$var, imp = summary(gbmModel_id_5)$rel.inf)
gbmModel_id_5_imp$id <- nrow(gbmModel_id_5_imp):1

gbmModel_id_9_imp <- data.frame(var = summary(gbmModel_id_9)$var, imp = summary(gbmModel_id_9)$rel.inf)
gbmModel_id_9_imp$id <- nrow(gbmModel_id_9_imp):1

gbmModel_id_15_imp <- data.frame(var = summary(gbmModel_id_15)$var, imp = summary(gbmModel_id_15)$rel.inf)
gbmModel_id_15_imp$id <- nrow(gbmModel_id_15_imp):1



# ----------
# As interaction depth increase, the slope of predictor importace gets steeper
graphics.off();  par(mfrow=c(1,1));
dotplot(id ~ imp, data = gbmModel_id_1_imp[1:20,], horizontal = TRUE, xlim = c(0,30))
dotplot(id ~ imp, data = gbmModel_id_5_imp[1:20,], horizontal = TRUE, xlim = c(0,30))
dotplot(id ~ imp, data = gbmModel_id_9_imp[1:20,], horizontal = TRUE, xlim = c(0,30))
dotplot(id ~ imp, data = gbmModel_id_15_imp[1:20,], horizontal = TRUE, xlim = c(0,30))



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
# testRes_rf <- data.frame(obs = solTestY, rf = predict(rfTune, solTestXtrans))
# testRes_rf <- testRes_rf %>% mutate(resid = obs - rf)

testRes_gbm <- data.frame(obs = solTestY, gbm = predict(gbmTune, solTestXtrans))
testRes_gbm <- testRes_gbm %>% mutate(resid = obs - gbm)

testRes_gbm01 <- data.frame(obs = solTestY, gbm01 = predict(gbmModel_01, n.trees = 600, solTestXtrans))
testRes_gbm01 <- testRes_gbm01 %>% mutate(resid = obs - gbm01)

testRes_gbm09 <- data.frame(obs = solTestY, gbm09 = predict(gbmModel_09, n.trees = 200, solTestXtrans))
testRes_gbm09 <- testRes_gbm09 %>% mutate(resid = obs - gbm09)

testRes_gbm_id_1 <- data.frame(obs = solTestY, gbm_id_1 = predict(gbmModel_id_1, n.trees = 500, solTestXtrans))
testRes_gbm_id_1 <- testRes_gbm_id_1 %>% mutate(resid = obs - gbm_id_1)

testRes_gbm_id_5 <- data.frame(obs = solTestY, gbm_id_5 = predict(gbmModel_id_5, n.trees = 500, solTestXtrans))
testRes_gbm_id_5 <- testRes_gbm_id_5 %>% mutate(resid = obs - gbm_id_5)

testRes_gbm_id_9 <- data.frame(obs = solTestY, gbm_id_9 = predict(gbmModel_id_9, n.trees = 500, solTestXtrans))
testRes_gbm_id_9 <- testRes_gbm_id_9 %>% mutate(resid = obs - gbm_id_9)

testRes_gbm_id_15 <- data.frame(obs = solTestY, gbm_id_15 = predict(gbmModel_id_15, n.trees = 500, solTestXtrans))
testRes_gbm_id_15 <- testRes_gbm_id_15 %>% mutate(resid = obs - gbm_id_15)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testRes_gbm$obs, testRes_gbm$gbm, testRes_gbm01$gbm01, testRes_gbm09$gbm09, testRes_gbm_id_1$gbm_id_1))

graphics.off()
par(mfrow = c(2,2))
# with(testRes_rf, plot(obs, rf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
# with(testRes_rf, plot(rf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_gbm, plot(obs, gbm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_gbm, plot(gbm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_gbm01, plot(obs, gbm01, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_gbm01, plot(gbm01, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_gbm09, plot(obs, gbm09, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_gbm09, plot(gbm09, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_gbm_id_1, plot(obs, gbm_id_1, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_gbm_id_1, plot(gbm_id_1, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_gbm_id_5, plot(obs, gbm_id_5, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_gbm_id_5, plot(gbm_id_5, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_gbm_id_9, plot(obs, gbm_id_9, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_gbm_id_9, plot(gbm_id_9, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_gbm_id_15, plot(obs, gbm_id_15, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_gbm_id_15, plot(gbm_id_15, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)




# ------------------------------------------------------------------------------
# train boosted trees by gbm
# ------------------------------------------------------------------------------
# distribution:  the type of loss function that will be optimized during boosting.
# For a continuous response, distribution should be set to "gaussian".

gbmModel <- gbm.fit(solTrainXtrans, solTrainY, distribution = "gaussian")

# or
gbmModel <- gbm(y ~., data = trainData, distribution = "gaussian")

