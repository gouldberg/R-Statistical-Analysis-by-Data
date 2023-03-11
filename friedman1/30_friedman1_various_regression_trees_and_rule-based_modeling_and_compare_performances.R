setwd("//media//kswada//MyFiles//R//friedman1")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "mlbench")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



################################################################################
# SUMMARY
#
# If the predictor range of test data is within that of training data, the mean of RMSE best models:
#   - Boosted Trees --> Cubist --> Random Forest --> Bagged Trees --> Conditional Inference Trees --> CART
#   - Boosted Trees and Cubist models are significantly better than others, when the predictor range of test data is within that of training data
# But the predictor range of test data is out of range of training data, NONE of above models does good job ... for this data
#   - Only Cubist has variation in predicted values (other models have only ONE predicted values, but for different by each model)
#
# The importance measure of Random Forest (CART) is significantly affected by highly correlated other predictors 
# But importance measure of Random Forest with conditional inference trees , Boosted Trees and Cubist are not affeted much.
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
# OPTION:  Add an additional predictor that is highly correlated with one of the informative predictors
trainData$x[,"duplicate1"] <- trainData$x[,"X1"] + rnorm(200) * 0.1
cor(trainData$x[,"duplicate1"], trainData$x[,"X1"])



# ----------
# NOTE that X6 - X10 is uninformative predictors
# IT is expected that random forest model does significantly use those uninformative predictors
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
# regression trees and rule-based modeling by various models with automatic hyper-parameter tuning and with cross-validation
# ------------------------------------------------------------------------------
# CART
set.seed(100)
cartTune <- train(x = trainData$x, y = trainData$y, method = "rpart", tuneLength = 25, trControl = ctrl)

plot(cartTune, scales = list(x = list(log = 10)))

cartTune
cartTune$finalModel


plot(as.party(cartTune$finalModel))



# ----------
# Conditional Inference Trees
( cGrid <- data.frame(mincriterion = sort(c(.95, seq(.75, .99, length = 15)))) )

set.seed(100)
ctreeTune <- train(x = trainData$x, y = trainData$y, method = "ctree", tuneGrid = cGrid, trControl = ctrl)

plot(ctreeTune)

ctreeTune
ctreeTune$finalModel               


plot(ctreeTune$finalModel)



# ----------
# Bagged Trees
library(doMC)
registerDoMC(10)

set.seed(100)
treebagTune <- train(x = trainData$x, y = trainData$y, method = "treebag", nbagg = 50, trControl = ctrl)

treebagTune

treebagImp <- varImp(treebagTune, scale = TRUE)
treebagImp
plot(treebagImp, top = 10)



# ----------
# Random Forest (CART)
library(doMC)
registerDoMC(10)

( mtryGrid <- data.frame(mtry = floor(seq(1, ncol(trainData$x), length = 10))) )

set.seed(100)
rfTune <- train(x = trainData$x, y = trainData$y, method = "rf", tuneGrid = mtryGrid,
                ntree = 1000, importance = TRUE, trControl = ctrl)

plot(rfTune)

rfTune


# NOTE that X6 - X10 is uninformative predictors
# IT is expected that random forest model does significantly use those uninformative predictors
# Random Forest with CART shows X1's importance smaller when "duplicate1" is included in the predictors
rfImp <- varImp(rfTune, scale = FALSE)
plot(rfImp)



# ----------
# Random Forest (Conditional Inference Tree: cforest)
library(party)

set.seed(100)

condrfTune <- train(x = trainData$x, y = trainData$y, method = "cforest", tuneGrid = mtryGrid,
                    controls = party::cforest_unbiased(ntree = 1000), trControl = ctrl)

plot(condrfTune)

condrfTune


# Random Forest with conditional inference trees still shows X1's importance is large even if "duplicate1" is included in the predictors
condrfImp <- varImp(condrfTune, scale = FALSE)
plot(condrfImp, top = 10)



# ----------
# Boosted Trees
library(doMC)
registerDoMC(10)

gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2), n.trees = seq(100, 1000, by = 50), 
                       shrinkage = c(0.01, 0.1), n.minobsinnode = 1)


set.seed(100)
gbmTune <- train(x = trainData$x, y = trainData$y, method = "gbm",
                 tuneGrid = gbmGrid, trControl = ctrl, verbose = TRUE)

gbmTune

plot(gbmTune, auto.key = list(columns = 4, lines = TRUE))


# boosted trees model still shows X1's importance as largest even if "duplicate1" is included in the predictors
gbmImp <- varImp(gbmTune, scale = FALSE)
gbmImp
plot(gbmImp)



# ----------
# Cubist (model tree)
library(doMC)
registerDoMC(10)

cbGrid <- expand.grid(committees = c(1:10, 20, 50, 75, 100), neighbors = c(0, 1, 5, 9))

set.seed(100)

cubistTune <- train(x = trainData$x, y = trainData$y, method = "cubist",
                    tuneGrid = cbGrid, trControl = ctrl)

plot(cubistTune, auto.key = list(columns = 4, lines = TRUE))

cubistTune
cubistTune$finalModel


# Cubist model still shows X1's importance as largest even if "duplicate1" is included in the predictors
cbImp <- varImp(cubistTune, scale = FALSE)
cbImp
plot(cbImp)



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
trainRes_cart <- data.frame(obs = trainData$y, cart = predict(cartTune, trainData$x)) %>% mutate(resid = obs - cart)
trainRes_treebag <- data.frame(obs = trainData$y, treebag = predict(treebagTune, trainData$x)) %>% mutate(resid = obs - treebag)
trainRes_rf <- data.frame(obs = trainData$y, rf = predict(rfTune, trainData$x)) %>% mutate(resid = obs - rf)
trainRes_condrf <- data.frame(obs = trainData$y, condrf = predict(condrfTune, trainData$x)) %>% mutate(resid = obs - condrf)
trainRes_gbm <- data.frame(obs = trainData$y, gbm = predict(gbmTune, trainData$x)) %>% mutate(resid = obs - gbm)
trainRes_cubist <- data.frame(obs = trainData$y, cubist = predict(cubistTune, trainData$x)) %>% mutate(resid = obs - cubist)



# ----------
# diagnositc plot
axisRange <- extendrange(c(trainRes_cart$obs, trainRes_cart$cart, trainRes_treebag$treebag, trainRes_rf$rf,
                           trainRes_condrf$condrf, trainRes_gbm$gbm, trainRes_cubist$cubist))

graphics.off()
par(mfrow = c(2,2))
with(trainRes_cart, plot(obs, cart, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_cart, plot(cart, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainRes_treebag, plot(obs, treebag, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_treebag, plot(treebag, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainRes_rf, plot(obs, rf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_rf, plot(rf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainRes_condrf, plot(obs, condrf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_condrf, plot(condrf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(trainRes_gbm, plot(obs, gbm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_gbm, plot(gbm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainRes_cubist, plot(obs, cubist, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_cubist, plot(cubist, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



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

model_list <- list(cart = cartTune, treebag = treebagTune, rf = rfTune, condrf = condrfTune, gbm = gbmTune, cubist = cubistTune)

resamp <- resamples(model_list)

summary(resamp)


# -->
# mean of RMSE:  top models
# gbm --> cubist --> rf --> treebag --> condrf --> cart
# gbm and cubist models are significantly better than others ....



# ----------
# resampled distribution of metric
trellis.par.set(theme1)
par(mfrow=c(1,1))
bwplot(resamp, auto.key = list(columns = 6))

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
library(magrittr)  # for set_names

cartPred <- predict(cartTune, newdata = testData$x)
treebagPred <- predict(treebagTune, newdata = testData$x)
rfPred <- predict(rfTune, newdata = testData$x)
condrfPred <- predict(condrfTune, newdata = testData$x)
gbmPred <- predict(gbmTune, newdata = testData$x)
cubistPred <- predict(cubistTune, newdata = testData$x)


cart_perf <- postResample(pred = cartPred, obs = testData$y) %>% data.frame() %>% set_names("cart")
treebag_perf <- postResample(pred = treebagPred, obs = testData$y) %>% data.frame() %>% set_names("treebag")
rf_perf <- postResample(pred = rfPred, obs = testData$y) %>% data.frame() %>% set_names("rf")
condrf_perf <- postResample(pred = condrfPred, obs = testData$y) %>% data.frame() %>% set_names("condrf")
gbm_perf <- postResample(pred = gbmPred, obs = testData$y) %>% data.frame() %>% set_names("gbm")
cubist_perf <- postResample(pred = cubistPred, obs = testData$y) %>% data.frame() %>% set_names("cubist")

all_perf <- cbind(cart_perf, treebag_perf, rf_perf, condrf_perf, gbm_perf, cubist_perf)

round(all_perf, digits = 3)



# ------------------------------------------------------------------------------
# check prediction by each model
# ------------------------------------------------------------------------------
testRes_cart <- data.frame(obs = testData$y, cart = predict(cartTune, newdata = testData$x)) %>% mutate(resid = obs - cart)
testRes_treebag <- data.frame(obs = testData$y, treebag = predict(treebagTune, newdata = testData$x)) %>% mutate(resid = obs - treebag)
testRes_rf <- data.frame(obs = testData$y, rf = predict(rfTune, newdata = testData$x)) %>% mutate(resid = obs - rf)
testRes_condrf <- data.frame(obs = testData$y, condrf = predict(condrfTune, newdata = testData$x)) %>% mutate(resid = obs - condrf)
testRes_gbm <- data.frame(obs = testData$y, gbm = predict(gbmTune, newdata = testData$x)) %>% mutate(resid = obs - gbm)
testRes_cubist <- data.frame(obs = testData$y, cubist = predict(cubistTune, newdata = testData$x)) %>% mutate(resid = obs - cubist)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testRes_cart$obs, testRes_cart$cart, testRes_treebag$treebag, testRes_rf$rf,
                           testRes_condrf$condrf, testRes_gbm$gbm, testRes_cubist$cubist))

graphics.off()
par(mfrow = c(2,2))
with(testRes_cart, plot(obs, cart, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_cart, plot(cart, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_treebag, plot(obs, treebag, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_treebag, plot(treebag, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(testRes_rf, plot(obs, rf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_rf, plot(rf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_condrf, plot(obs, condrf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_condrf, plot(condrf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)


with(testRes_gbm, plot(obs, gbm, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_gbm, plot(gbm, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_cubist, plot(obs, cubist, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_cubist, plot(cubist, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



