setwd("//media//kswada//MyFiles//R//tecator")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tecator
# ------------------------------------------------------------------------------
data("tecator", package = "caret")

dim(absorp)
dim(endpoints)

car::some(absorp)
car::some(endpoints)



# ----------
trainData <- list()
trainData$x <- data.frame(absorp)
trainData$y <- endpoints[,2]



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
# robust regression  --> failed to converge in 20 steps
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
pcrTune <- train(x = trainData$x, y = trainData$y, method = "pcr", tuneGrid = expand.grid(ncomp = 1:20), trControl = ctrl)

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
# neural networks with model averaging:  IT TAKES TIME !!!!: 10 -^ 20 minutes
library(doMC)
registerDoMC(10)
set.seed(100)


nnetGrid <- expand.grid(decay = c(0, 0.001, 0.01, .1),  size = c(1, 3, 5, 7, 9, 11, 13),  bag = FALSE)

nnetTune <- train(x = trainData$x, y = trainData$y, method = "avNNet", tuneGrid = nnetGrid, trControl = ctrl,
                  preProc = c("center", "scale"), linout = TRUE, trace = TRUE,
                  MaxNWts = 13 * (ncol(trainData$x) + 1) + 13 + 1,
                  maxit = 1000, allowParallel = TRUE)


# Apply preprocess with PCA, since neural networks are especially sensitive to highly correlated prediuctors
# --> but the performance was worst ... why ?
trans <- preProcess(trainData$x, c("pca"))
dat_trans <- predict(trans, trainData$x)

nnetGrid <- expand.grid(decay = c(0, 0.001, 0.01, .1),  size = c(1, 3, 5, 7, 9, 11, 13),  bag = FALSE)

nnetTune_pca <- train(x = dat_trans, y = trainData$y, method = "avNNet", tuneGrid = nnetGrid, trControl = ctrl,
                  preProc = c("center", "scale"), linout = TRUE, trace = TRUE,
                  MaxNWts = 13 * (ncol(dat_trans) + 1) + 13 + 1,
                  maxit = 1000, allowParallel = TRUE)


nnetTune$finalModel
nnetTune_pca$finalModel

plot(nnetTune)
plot(nnetTune_pca)

varImp(nnetTune)
varImp(nnetTune_pca)

plotmo(nnetTune)
plotmo(nnetTune_pcs)



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
trainingRes_nnet_pca <- data.frame(obs = trainData$y, nnet_pca = predict(nnetTune_pca, dat_trans)) %>% mutate(resid = obs - nnet_pca)
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
with(trainingRes_nnet_pca, plot(obs, nnet_pca, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_nnet_pca, plot(nnet_pca, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainingRes_rsvm, plot(obs, rsvm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_rsvm, plot(rsvm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainingRes_psvm, plot(obs, psvm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_psvm, plot(psvm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainingRes_knn, plot(obs, knn, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainingRes_knn, plot(knn, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
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
                   nnet = nnetTune, nnet_pca = nnetTune_pca, knn = knnTune, rsvm = svmRTune, psvm = svmPTune, mars = marsTune)

resamp <- resamples(model_list)

summary(resamp)


# -->
# mean of RMSE:  top 6 model
# nnet --> psvm --> mars --> pls --> pcr --> enet


# ----------
# resampled distribution of metric
trellis.par.set(theme1)
par(mfrow=c(1,1))
bwplot(resamp, auto.key = list(columns = 12))

trellis.par.set(caretTheme())
densityplot(resamp, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 2), adjust = 1.5)



# ----------
modelDifferences <- diff(resamp)
summary(modelDifferences)



# ----------
# The actual paired t-test:
modelDifferences$statistics$RMSE


