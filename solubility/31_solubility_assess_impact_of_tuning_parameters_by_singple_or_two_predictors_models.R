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



# ------------------------------------------------------------------------------
# Select only one predictor or only 2 predictors
# ------------------------------------------------------------------------------
trainX <- data.frame(X1 = solTrainXtrans$MolWeight)

trainX2 <- data.frame(X1 = solTrainXtrans$MolWeight, X2 = solTrainXtrans$NumCarbon)


testX <- data.frame(X1 = solTestXtrans$MolWeight)

testX2 <- data.frame(X1 = solTestXtrans$MolWeight, X2 = solTestXtrans$NumCarbon)



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
# Assess impact of tuning parameters by sigle predictor model
# ------------------------------------------------------------------------------
# linear regression
set.seed(100)
lmTune <- train(x = trainX, y = solTrainY, method = "lm", trControl = ctrl)
lmTune2 <- train(x = trainX2, y = solTrainY, method = "lm", trControl = ctrl)

lmTune                
lmTune2

summary(lmTune)
summary(lmTune2)



# ----------
# random forest model
library(doMC)
registerDoMC(10)

set.seed(100)
rfTune <- train(x = trainX, y = solTrainY, method = "rf",
                ntree = 1000, importance = TRUE, trControl = ctrl)

rfTune2 <- train(x = trainX2, y = solTrainY, method = "rf",
                ntree = 1000, importance = TRUE, trControl = ctrl)

rfTune

rfTune2



# ----------
# Cubist model with a single rule or multiple committees
cbGrid <- expand.grid(committees = 1, neighbors = c(0, 1, 5, 9))
cbGrid2 <- expand.grid(committees = 1, neighbors = 3)
cbGrid3 <- expand.grid(committees = c(1:10, 20), neighbors = c(0, 1, 5, 9))

set.seed(100)
cubistTune <- train(x = trainX, y = solTrainY, method = "cubist",
                    tuneGrid = cbGrid, trControl = ctrl)

cubistTune2 <- train(x = trainX2, y = solTrainY, method = "cubist",
                     tuneGrid = cbGrid2, trControl = ctrl)

cubistTune3 <- train(x = trainX2, y = solTrainY, method = "cubist",
                     tuneGrid = cbGrid3, trControl = ctrl)

cubistTune

cubistTune2

cubistTune3


cubistTune$finalModel

cubistTune2$finalModel

cubistTune3$finalModel



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
trainRes_lm <- data.frame(obs = solTrainY, lm = predict(lmTune, trainX)) %>% mutate(resid = obs - lm)
trainRes_lm2 <- data.frame(obs = solTrainY, lm2 = predict(lmTune2, trainX2)) %>% mutate(resid = obs - lm2)

trainRes_rf <- data.frame(obs = solTrainY, rf = predict(rfTune, trainX)) %>% mutate(resid = obs - rf)
trainRes_rf2 <- data.frame(obs = solTrainY, rf2 = predict(rfTune2, trainX2)) %>% mutate(resid = obs - rf2)

trainRes_cubist <- data.frame(obs = solTrainY, cubist = predict(cubistTune, trainX)) %>% mutate(resid = obs - cubist)
trainRes_cubist2 <- data.frame(obs = solTrainY, cubist2 = predict(cubistTune2, trainX2)) %>% mutate(resid = obs - cubist2)
trainRes_cubist3 <- data.frame(obs = solTrainY, cubist3 = predict(cubistTune2, trainX2)) %>% mutate(resid = obs - cubist3)


# ----------
# diagnositc plot
axisRange <- extendrange(c(trainRes_lm$obs, trainRes_lm$lm, trainRes_rf$rf, trainRes_cubist$cubist))

graphics.off()
par(mfrow = c(3,2))
with(trainRes_lm, plot(obs, lm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_lm, plot(lm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(trainRes_rf, plot(obs, rf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_rf, plot(rf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(trainRes_cubist, plot(obs, cubist, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_cubist, plot(cubist, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(trainRes_lm2, plot(obs, lm2, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_lm2, plot(lm2, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(trainRes_rf2, plot(obs, rf2, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_rf2, plot(rf2, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(trainRes_cubist2, plot(obs, cubist2, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_cubist2, plot(cubist2, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(trainRes_cubist3, plot(obs, cubist3, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_cubist3, plot(cubist3, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



# ------------------------------------------------------------------------------
# Choosing Between Models
# ------------------------------------------------------------------------------

# best model assesed by mean of RMSE:  rf2 --> cubist3 --> cubist2 --> rf --> lm2 --> cubist --> lm
# If you choose right predictors, Random Forest (CART) does good job in training data
# For Cubist, parameter changes does not affect much (if you choose right predictors)

model_list <- list(lm = lmTune, lm2 = lmTune2, rf = rfTune, rf2 = rfTune2, cubist = cubistTune, cubist2 = cubistTune2, cubist3 = cubistTune3)

resamp <- resamples(model_list)

summary(resamp)



# ----------
modelDifferences <- diff(resamp)
summary(modelDifferences)



# ----------
# The actual paired t-test:
modelDifferences$statistics$RMSE



# ------------------------------------------------------------------------------
# Compare predicted performance
#
# "Note that many models have more predictors (or parameters) than data points, so the typical mean squared error denominator (n - p) does not
# apply. Root mean squared error is calculated using sqrt(mean((pred - obs)^2)). Also, R-squared is calculated as the square of the
# correlation between the observed and predicted outcomes."

# Since caret is useful for comparing different types of models, we use biased estimate of the root MSE since we would like to directly
# compare the RMSE from different models (say a linear regression and a support vector machine).
# Many of these models do not have an explicit number of parameters, so we use
# mse <- mean((pred - obs)^2)
# ------------------------------------------------------------------------------
lmPred <- predict(lmTune, newdata = testX)
lmPred2 <- predict(lmTune2, newdata = testX2)

rfPred <- predict(rfTune, newdata = testX)
rfPred2 <- predict(rfTune2, newdata = testX2)

cubistPred <- predict(cubistTune, newdata = testX)
cubistPred2 <- predict(cubistTune2, newdata = testX2)
cubistPred3 <- predict(cubistTune3, newdata = testX2)


lm_perf <- postResample(pred = lmPred, obs = solTestY) %>% data.frame() %>% set_names("lm")
lm_perf2 <- postResample(pred = lmPred2, obs = solTestY) %>% data.frame() %>% set_names("lm2")
rf_perf <- postResample(pred = rfPred, obs = solTestY) %>% data.frame() %>% set_names("rf")
rf_perf2 <- postResample(pred = rfPred2, obs = solTestY) %>% data.frame() %>% set_names("rf2")
cubist_perf <- postResample(pred = cubistPred, obs = solTestY) %>% data.frame() %>% set_names("cubist")
cubist_perf2 <- postResample(pred = cubistPred2, obs = solTestY) %>% data.frame() %>% set_names("cubist2")
cubist_perf3 <- postResample(pred = cubistPred3, obs = solTestY) %>% data.frame() %>% set_names("cubist3")


all_perf <- cbind(lm_perf, lm_perf2, rf_perf, rf_perf2, cubist_perf, cubist_perf2, cubist_perf3)


# ----------
# best model assesed by mean of RMSE:  lm --> cubist --> lm2 --> rf --> cubist2 --> cubist3 --> rf2

round(all_perf, digits = 3)



# ------------------------------------------------------------------------------
# check prediction by each model
# ------------------------------------------------------------------------------
testRes_lm <- data.frame(obs = solTestY, lm = predict(lmTune, newdata = testX)) %>% mutate(resid = obs - lm)
testRes_lm2 <- data.frame(obs = solTestY, lm2 = predict(lmTune2, newdata = testX2)) %>% mutate(resid = obs - lm2)

testRes_rf <- data.frame(obs = solTestY, rf = predict(rfTune, newdata = testX)) %>% mutate(resid = obs - rf)
testRes_rf2 <- data.frame(obs = solTestY, rf2 = predict(rfTune2, newdata = testX2)) %>% mutate(resid = obs - rf2)

testRes_cubist <- data.frame(obs = solTestY, cubist = predict(cubistTune, newdata = testX)) %>% mutate(resid = obs - cubist)
testRes_cubist2 <- data.frame(obs = solTestY, cubist2 = predict(cubistTune2, newdata = testX2)) %>% mutate(resid = obs - cubist2)
testRes_cubist3 <- data.frame(obs = solTestY, cubist3 = predict(cubistTune2, newdata = testX2)) %>% mutate(resid = obs - cubist3)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testRes_lm$obs, testRes_lm$lm, testRes_rf$rf, testRes_cubist$cubist))

graphics.off()
par(mfrow = c(3,2))
with(testRes_lm, plot(obs, lm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_lm, plot(lm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_rf, plot(obs, rf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_rf, plot(rf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_cubist, plot(obs, cubist, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_cubist, plot(cubist, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_lm2, plot(obs, lm2, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_lm2, plot(lm2, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_rf2, plot(obs, rf2, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_rf2, plot(rf2, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_cubist2, plot(obs, cubist2, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_cubist2, plot(cubist2, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_cubist3, plot(obs, cubist3, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_cubist3, plot(cubist3, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
