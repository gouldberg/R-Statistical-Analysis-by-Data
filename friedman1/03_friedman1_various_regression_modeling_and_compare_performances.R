setwd("//media//kswada//MyFiles//R//friedman1")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "mlbench")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



################################################################################
# SUMMARY
#
# If the predictor range of test data is within that of training data,  USE MARS
# If the predictor range of test data is out of range of training data,  USE PSVM
# MARS is significantly better than others, when the predictor range of test data is within that of training data
# PSVM is good at both cases
# ------------------------------------------------------------------------------
#  - training data performance (RMSE) top 5:  MARS --> psvm --> rsvm --> nnet --> pls
#      - MARS is significantly better than others
#
#  - test data performance (RMSE) top 5:  MARS --> psvm --> rsvm --> nnet --> pls / rlm
#      - MARS is significantly better than others
#
#  - For pls prediction for test data, overall RMSE is as same as that of pcr, but residuals of pls shows no good sign (under-fitting)
#      --> pls is not good for prediction (pls's number of principal components is much smaller than pcr)
#
#  - test data with PARTIALLY DIFFERENT RANGE of explanatory variables, performance (RMSE) top 5:  psvm --> nnet --> enet --> lm / pcr / pls
#      - MARS is worst !!!
#      - MARS is good at prediction at same range but worst at different range
#
#  - test data with TOTALLY DIFFERENT RANGE of explanatory variables, performance (RMSE) top 5:  psvm --> rml --> lm / pls / pcr --> ridge --> enet
#      - MARS is worst !!!  also nnet, rsvm are not good.
################################################################################


# ------------------------------------------------------------------------------
# data: friedman1
# ------------------------------------------------------------------------------
set.seed(200)

trainData <- mlbench.friedman1(200, sd = 1)

dim(trainData$x)

trainData$y


# ----------
# convert matrix to dafa frame, this will give the columns names
trainData$x <- data.frame(trainData$x)



# ----------
featurePlot(trainData$x, trainData$y,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))



# ----------
# This creats a list with a vector y and a matrix of predictors x, aso simulate a large test set to estimate the true error rate with good precision
testData <- mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x)

summary(testData$y)
summary(testData$x)



# ----------
# OR we create training and test data manually
gen_simdata <- function(n_sample = n_sample, minval = 0, maxval = maxval){
  X1 <- runif(n = n_sample, min = minval, max = maxval)
  X2 <- runif(n = n_sample, min = minval, max = maxval)
  X3 <- runif(n = n_sample, min = minval, max = maxval)
  X4 <- runif(n = n_sample, min = minval, max = maxval)
  X5 <- runif(n = n_sample, min = minval, max = maxval)
  X6 <- runif(n = n_sample, min = minval, max = maxval)
  X7 <- runif(n = n_sample, min = minval, max = maxval)
  X8 <- runif(n = n_sample, min = minval, max = maxval)
  X9 <- runif(n = n_sample, min = minval, max = maxval)
  X10 <- runif(n = n_sample, min = minval, max = maxval)
  X11 <- rnorm(n = n_sample, mean = 0, sd = maxval - minval)
  tmp_y <- 10 * sin(pi * X1 * X2) + 20 * (X3 - 0.5)^2 + 10 * X4 + 5 * X5 + X11
  tmp_x <- cbind(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)

  dat <- list()
  dat$x <- tmp_x
  dat$y <- tmp_y
  return(dat)
}

n_sample = 200
minval = 0;  maxval = 1;
trainData <- gen_simdata(n_sample, minval, maxval)

n_sample = 5000
minval = 1;  maxval = 2;
testData <- gen_simdata(n_sample, minval, maxval)

summary(trainData$y)
summary(trainData$x)

summary(testData$y)
summary(testData$x)

featurePlot(trainData$x, trainData$y,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))

featurePlot(testData$x, testData$y,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))


# ----------
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .3, .4, .5)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)



# ------------------------------------------------------------------------------
# Create folds explicitly
# ------------------------------------------------------------------------------
set.seed(100)


# create folds explicitly, default is 10 folds
indx <- createFolds(trainData$y, returnTrain = TRUE)
indx



# ------------------------------------------------------------------------------
# Set train control
# ------------------------------------------------------------------------------
ctrl <- trainControl(method = "cv", index = indx)



# ------------------------------------------------------------------------------
# regression modeling by various models with automatic hyper-parameter tuning and with cross-validation
# ------------------------------------------------------------------------------
# linear regression
set.seed(100)
lmTune <- train(x = trainData$x, y = trainData$y, method = "lm", trControl = ctrl)

lmTune$finalModel

summary(lmTune)

varImp(lmTune)

plotmo(lmTune)



# ----------
# robust regression
set.seed(100)
rlmTune <- train(x = trainData$x, y = trainData$y, method = "rlm", trControl = ctrl)

rlmTune$finalModel

summary(rlmTune)

varImp(rlmTune)

plotmo(rlmTune)



# ----------
# partial least squares regression
set.seed(100)
plsTune <- train(x = trainData$x, y = trainData$y, method = "pls", tuneGrid = expand.grid(ncomp = 1:10), trControl = ctrl)

plsTune

plot(plsTune)

summary(plsTune)

varImp(plsTune)

plotmo(plsTune)



# ----------
# principal component regression
set.seed(100)
pcrTune <- train(x = trainData$x, y = trainData$y, method = "pcr", tuneGrid = expand.grid(ncomp = 1:10), trControl = ctrl)

pcrTune

plot(pcrTune)

summary(pcrTune)

varImp(pcrTune)

plotmo(pcrTune)



# ----------
# ridge regression
set.seed(100)
ridgeTune <- train(x = trainData$x, y = trainData$y, method = "ridge", tuneGrid = expand.grid(lambda = seq(0, .1, length = 15)), 
                 preproc = c("center", "scale"), trControl = ctrl)

ridgeTune

plot(ridgeTune)

varImp(ridgeTune)

plotmo(ridgeTune)



# ----------
# Lasso
set.seed(100)
enetGrid <- expand.grid(lambda = c(0, 0.01, .1), fraction = seq(.05, 1, length = 20))
enetTune <- train(x = trainData$x, y = trainData$y, method = "enet", tuneGrid = enetGrid, 
                   preproc = c("center", "scale"), trControl = ctrl)

enetTune

plot(enetTune)

varImp(enetTune)

plotmo(enetTune)



# ----------
# neural networks with model averaging
library(doMC)
registerDoMC(10)
set.seed(100)

nnetGrid <- expand.grid(decay = c(0, 0.01, .1),  size = c(1, 3, 5, 7, 9, 11, 13),  bag = FALSE)
nnetTune <- train(x = trainData$x, y = trainData$y, method = "avNNet", tuneGrid = nnetGrid, trControl = ctrl,
                  preProc = c("center", "scale"), linout = TRUE, trace = TRUE,
                  MaxNWts = 13 * (ncol(trainData$x) + 1) + 13 + 1,
                  maxit = 1000, allowParallel = TRUE)

nnetTune$finalModel

plot(nnetTune)

varImp(nnetTune)

plotmo(nnetTune)



# ----------
# support vector machine regression
svmRTune <- train(x = trainData$x, y = trainData$y, method = "svmRadial", preProc = c("center", "scale"),
                  tuneLength = 14, trControl = ctrl)

svmRTune$finalModel

plot(svmRTune, scales = list(x = list(log = 2)))                 

varImp(svmRTune)



# ----------
# polynomial support vector machine regression
svmGrid <- expand.grid(degree = 1:2, scale = c(0.1, 0.01, 0.005, 0.001), C = 2^(-2:5))
set.seed(100)
svmPTune <- train(x = trainData$x, y = trainData$y, method = "svmPoly", preProc = c("center", "scale"),
                  tuneGrid = svmGrid, trControl = ctrl)

svmPTune$finalModel

plot(svmPTune, scales = list(x = list(log = 2), between = list(x = .5, y = 1)))                 

varImp(svmPTune)



# ----------
# KNN
set.seed(100)
knnTune <- train(x = trainData$x, y = trainData$y, method = "knn", preProc = c("center", "scale"),
                 tuneGrid = data.frame(k = 1:20), trControl = ctrl)

knnTune$finalModel

plot(knnTune)

varImp(knnTune)

plotmo(knnTune)



# ----------
# MARS
set.seed(100)
marsTune <- train(x = trainData$x, y = trainData$y, method = "earth", tuneGrid = expand.grid(degree = 1:2, nprune = 2:40), trControl = ctrl)

marsTune$finalModel

plot(marsTune)

summary(marsTune)

varImp(marsTune)

plotmo(marsTune)



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
trainingRes_lm <- data.frame(obs = trainData$y, lm = predict(lmTune, trainData$x)) %>% mutate(resid = obs - lm)
trainingRes_rlm <- data.frame(obs = trainData$y, rlm = predict(rlmTune, trainData$x)) %>% mutate(resid = obs - rlm)
trainingRes_pls <- data.frame(obs = trainData$y, pls = predict(plsTune, trainData$x)) %>% mutate(resid = obs - pls)
trainingRes_pcr <- data.frame(obs = trainData$y, pcr = predict(pcrTune, trainData$x)) %>% mutate(resid = obs - pcr)
trainingRes_ridge <- data.frame(obs = trainData$y, ridge = predict(ridgeTune, trainData$x)) %>% mutate(resid = obs - ridge)
trainingRes_enet <- data.frame(obs = trainData$y, enet = predict(enetTune, trainData$x)) %>% mutate(resid = obs - enet)
trainingRes_nnet <- data.frame(obs = trainData$y, nnet = predict(nnetTune, trainData$x)) %>% mutate(resid = obs - nnet)
trainingRes_rsvm <- data.frame(obs = trainData$y, rsvm = predict(svmRTune, trainData$x)) %>% mutate(resid = obs - rsvm)
trainingRes_psvm <- data.frame(obs = trainData$y, psvm = predict(svmPTune, trainData$x)) %>% mutate(resid = obs - psvm)
trainingRes_knn <- data.frame(obs = trainData$y, knn = predict(knnTune, trainData$x)) %>% mutate(resid = obs - knn)
trainingRes_mars <- data.frame(obs = trainData$y, y = predict(marsTune, trainData$x)) %>% mutate(resid = obs - y)



# ----------
# diagnositc plot
axisRange <- extendrange(c(trainingRes_lm$obs, trainingRes_lm$lm, trainingRes_lm$pcr, trainingRes_ridge$ridge, trainingRes_nnet$nnet, trainingRes_knn$knn, trainingRes_mars$mars))

graphics.off()
par(mfrow = c(2,2))
with(trainingRes_lm, plot(obs, lm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_lm, plot(lm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainingRes_rlm, plot(obs, rlm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_rlm, plot(rlm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainingRes_pls, plot(obs, pls, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_pls, plot(pls, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainingRes_pcr, plot(obs, pcr, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_pcr, plot(pcr, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainingRes_ridge, plot(obs, ridge, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_ridge, plot(ridge, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainingRes_enet, plot(obs, enet, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_enet, plot(enet, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainingRes_nnet, plot(obs, nnet, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_nnet, plot(nnet, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainingRes_knn, plot(obs, knn, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_knn, plot(knn, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainingRes_rsvm, plot(obs, rsvm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_rsvm, plot(rsvm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainingRes_psvm, plot(obs, psvm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_psvm, plot(psvm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainingRes_mars, plot(obs, y, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_mars, plot(y, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



# ------------------------------------------------------------------------------
# Choosing Between Models
#   - Hothorn et. al. (2005) and Eugster et al. (2008) describe statistical methods for comparing methodologies
#     based on resampling results.
#     Since the accuracies were measured using identically resampled data sets, statistical methods for paired comparisons
#     can be used to determine if the differences between models are statistically significant.
#   - A paired t-test can be used to evaluate the hypothesis that the models have equivalent accuracirs (on average) or, 
#     analogously, that the mean difference in accuracy for the resampled data sets is zero.
# ------------------------------------------------------------------------------
# To compare two models based on their cross-validation statistics, the resamples function can be used with models
# that share a common set of resampled data sets.

model_list <- list(lm = lmTune, rlm = rlmTune, pls = plsTune, pcr = pcrTune, ridge = ridgeTune, enet = enetTune, 
                   nnet = nnetTune, knn = knnTune, rsvm = svmRTune, psvm = svmPTune, mars = marsTune)

resamp <- resamples(model_list)

summary(resamp)


# -->
# mean of RMSE:  top 5 model
# MARS --> psvm --> rsvm --> nnet --> pls
# MARS is significantly better than others ....



# ----------
# resampled distribution of metric
trellis.par.set(theme1)
par(mfrow=c(1,1))
bwplot(resamp, auto.key = list(columns = 11))

trellis.par.set(caretTheme())
densityplot(resamp, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 2), adjust = 1.5)



# ----------
modelDifferences <- diff(resamp)
summary(modelDifferences)



# ----------
# The actual paired t-test:
modelDifferences$statistics$RMSE



# ------------------------------------------------------------------------------
# Compare predicted performance
# ------------------------------------------------------------------------------
lmPred <- predict(lmTune, newdata = testData$x)
rlmPred <- predict(rlmTune, newdata = testData$x)
plsPred <- predict(plsTune, newdata = testData$x)
pcrPred <- predict(pcrTune, newdata = testData$x)
ridgePred <- predict(ridgeTune, newdata = testData$x)
enetPred <- predict(enetTune, newdata = testData$x)
nnetPred <- predict(nnetTune, newdata = testData$x)
rsvmPred <- predict(svmRTune, newdata = testData$x)
psvmPred <- predict(svmPTune, newdata = testData$x)
knnPred <- predict(knnTune, newdata = testData$x)
marsPred <- predict(marsTune, newdata = testData$x)


lm_perf <- postResample(pred = lmPred, obs = testData$y) %>% data.frame() %>% set_names("lm")
rlm_perf <- postResample(pred = rlmPred, obs = testData$y) %>% data.frame() %>% set_names("rlm")
pls_perf <- postResample(pred = plsPred, obs = testData$y) %>% data.frame() %>% set_names("pls")
pcr_perf <- postResample(pred = pcrPred, obs = testData$y) %>% data.frame() %>% set_names("pcr")
ridge_perf <- postResample(pred = ridgePred, obs = testData$y) %>% data.frame() %>% set_names("ridge")
enet_perf <- postResample(pred = enetPred, obs = testData$y) %>% data.frame() %>% set_names("enet")
nnet_perf <- postResample(pred = nnetPred, obs = testData$y) %>% data.frame() %>% set_names("nnet")
rsvm_perf <- postResample(pred = rsvmPred, obs = testData$y) %>% data.frame() %>% set_names("rsvm")
psvm_perf <- postResample(pred = psvmPred, obs = testData$y) %>% data.frame() %>% set_names("psvm")
knn_perf <- postResample(pred = knnPred, obs = testData$y) %>% data.frame() %>% set_names("knn")
mars_perf <- postResample(pred = marsPred, obs = testData$y) %>% data.frame() %>% set_names("mars")

all_perf <- cbind(lm_perf, rlm_perf, pls_perf, pcr_perf, ridge_perf, enet_perf, nnet_perf, rsvm_perf, psvm_perf, knn_perf, mars_perf)

round(all_perf, digits = 3)



# ------------------------------------------------------------------------------
# check prediction by each model
# ------------------------------------------------------------------------------
testRes_lm <- data.frame(obs = testData$y, lm = predict(lmTune, newdata = testData$x)) %>% mutate(resid = obs - lm)
testRes_rlm <- data.frame(obs = testData$y, rlm = predict(rlmTune, newdata = testData$x)) %>% mutate(resid = obs - rlm)
testRes_pls <- data.frame(obs = testData$y, pls = predict(plsTune, newadata = testData$x)) %>% mutate(resid = obs - pls)
testRes_pcr <- data.frame(obs = testData$y, pcr = predict(pcrTune, newdata = testData$x)) %>% mutate(resid = obs - pcr)
testRes_ridge <- data.frame(obs = testData$y, ridge = predict(ridgeTune, newdata = testData$x)) %>% mutate(resid = obs - ridge)
testRes_enet <- data.frame(obs = testData$y, enet = predict(enetTune, newdata = testData$x)) %>% mutate(resid = obs - enet)
testRes_nnet <- data.frame(obs = testData$y, nnet = predict(nnetTune, newdata = testData$x)) %>% mutate(resid = obs - nnet)
testRes_rsvm <- data.frame(obs = testData$y, rsvm = predict(svmRTune, newdata = testData$x)) %>% mutate(resid = obs - rsvm)
testRes_psvm <- data.frame(obs = testData$y, psvm = predict(svmPTune, newdata = testData$x)) %>% mutate(resid = obs - psvm)
testRes_knn <- data.frame(obs = testData$y, knn = predict(knnTune, newdata = testData$x)) %>% mutate(resid = obs - knn)
testRes_mars <- data.frame(obs = testData$y, y = predict(marsTune, newdata = testData$x)) %>% mutate(resid = obs - y)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testRes_lm$obs, testRes_lm$lm, testRes_lm$pcr, testRes_ridge$ridge, testRes_nnet$nnet, testRes_knn$knn, testRes_mars$y, testRes_mars$obs))

graphics.off()
par(mfrow = c(2,2))
with(testRes_lm, plot(obs, lm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_lm, plot(lm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_rlm, plot(obs, rlm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_rlm, plot(rlm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(testRes_pls, plot(obs, pls, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_pls, plot(pls, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_pcr, plot(obs, pcr, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_pcr, plot(pcr, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(testRes_ridge, plot(obs, ridge, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_ridge, plot(ridge, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_enet, plot(obs, enet, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_enet, plot(enet, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(testRes_nnet, plot(obs, nnet, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_nnet, plot(nnet, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_knn, plot(obs, knn, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_knn, plot(knn, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(testRes_rsvm, plot(obs, rsvm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_rsvm, plot(rsvm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_psvm, plot(obs, psvm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_psvm, plot(psvm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(testRes_mars, plot(obs, y, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_mars, plot(y, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



