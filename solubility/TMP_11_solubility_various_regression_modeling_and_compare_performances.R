setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "MASS", "elasticnet")
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


# ----------
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .3, .4, .5)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)



# ----------
trainData <- list()
trainData$x <- solTrainXtrans
trainData$y <- solTrainY

testData <- list()
testData$x <- solTestXtrans
testData$y <- solTestY



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
plsTune <- train(x = trainData$x, y = trainData$y, method = "pls", tuneGrid = expand.grid(ncomp = 1:20), trControl = ctrl)

plsTune

plot(plsTune)

summary(plsTune)

varImp(plsTune)

plotmo(plsTune)



# ----------
# principal component regression
set.seed(100)
pcrTune <- train(x = trainData$x, y = trainData$y, method = "pcr", tuneGrid = expand.grid(ncomp = 1:50), trControl = ctrl)

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
# neural networks with model averaging:  IT TAKES TIME !!!
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
trainRes_lm <- data.frame(obs = trainData$y, lm = predict(lmTune, trainData$x)) %>% mutate(resid = obs - lm)
trainRes_rlm <- data.frame(obs = trainData$y, rlm = predict(rlmTune, trainData$x)) %>% mutate(resid = obs - rlm)
trainRes_pls <- data.frame(obs = trainData$y, pls = predict(plsTune, trainData$x)) %>% mutate(resid = obs - pls)
trainRes_pcr <- data.frame(obs = trainData$y, pcr = predict(pcrTune, trainData$x)) %>% mutate(resid = obs - pcr)
trainRes_ridge <- data.frame(obs = trainData$y, ridge = predict(ridgeTune, trainData$x)) %>% mutate(resid = obs - ridge)
trainRes_enet <- data.frame(obs = trainData$y, enet = predict(enetTune, trainData$x)) %>% mutate(resid = obs - enet)
trainRes_nnet <- data.frame(obs = trainData$y, nnet = predict(nnetTune, trainData$x)) %>% mutate(resid = obs - nnet)
trainRes_rsvm <- data.frame(obs = trainData$y, rsvm = predict(svmRTune, trainData$x)) %>% mutate(resid = obs - rsvm)
trainRes_psvm <- data.frame(obs = trainData$y, psvm = predict(svmPTune, trainData$x)) %>% mutate(resid = obs - psvm)
trainRes_knn <- data.frame(obs = trainData$y, knn = predict(knnTune, trainData$x)) %>% mutate(resid = obs - knn)
trainRes_mars <- data.frame(obs = trainData$y, y = predict(marsTune, trainData$x)) %>% mutate(resid = obs - y)



# ----------
# diagnositc plot
axisRange <- extendrange(c(trainRes_lm$obs, trainRes_lm$lm, trainRes_lm$pcr, trainRes_ridge$ridge, trainRes_nnet$nnet, trainRes_knn$knn, trainRes_mars$mars))

graphics.off()
par(mfrow = c(2,2))
with(trainRes_lm, plot(obs, lm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_lm, plot(lm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainRes_rlm, plot(obs, rlm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_rlm, plot(rlm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainRes_pls, plot(obs, pls, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_pls, plot(pls, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainRes_pcr, plot(obs, pcr, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_pcr, plot(pcr, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainRes_ridge, plot(obs, ridge, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_ridge, plot(ridge, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainRes_enet, plot(obs, enet, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_enet, plot(enet, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainRes_nnet, plot(obs, nnet, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_nnet, plot(nnet, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainRes_knn, plot(obs, knn, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_knn, plot(knn, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainRes_rsvm, plot(obs, rsvm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_rsvm, plot(rsvm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainRes_psvm, plot(obs, psvm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_psvm, plot(psvm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainRes_mars, plot(obs, y, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_mars, plot(y, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



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



