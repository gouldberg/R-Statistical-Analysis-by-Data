rm(list=ls())
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

str(absorp)
str(endpoints)



# ------------------------------------------------------------------------------
# Preprocess data
# ------------------------------------------------------------------------------
trans <- preProcess(data.frame(absorp), method = c("BoxCox", "center", "scale", "pca"))
trans2 <- preProcess(data.frame(absorp), method = c("BoxCox", "center", "scale"))

trans
trans2

absorp_trans <- predict(trans, data.frame(absorp))
colnames(absorp_trans) <- c("PC1", "PC2")

absorp_trans2 <- predict(trans2, data.frame(absorp))

endpoints <- data.frame(endpoints)
colnames(endpoints) <- c("moisture", "fat", "protein")



# ------------------------------------------------------------------------------
# Split train and test data
# ------------------------------------------------------------------------------
absorp_trans$id <- 1:nrow(absorp_trans)
absorp_trans2$id <- 1:nrow(absorp_trans2)
endpoints$id <- 1:nrow(endpoints)


set.seed(100)

train_id <- sample(1:nrow(endpoints), size = floor(nrow(endpoints) * 0.75), replace = FALSE)
test_id <- setdiff(1:nrow(endpoints), train_id)

trainX <- absorp_trans %>% filter(id %in% train_id) %>% dplyr::select(-id)
testX <- absorp_trans %>% filter(id %in% test_id) %>% dplyr::select(-id)

trainX2 <- absorp_trans2 %>% filter(id %in% train_id) %>% dplyr::select(-id)
testX2 <- absorp_trans2 %>% filter(id %in% test_id) %>% dplyr::select(-id)



# ----------
# take "fat" for response
trainY <- endpoints %>% filter(id %in% train_id) %>% dplyr::select(fat)
testY <- endpoints %>% filter(id %in% test_id) %>% dplyr::select(fat)



# ----------
trainingData <- trainX
trainingData$fat <- trainY$fat

testData <- testX
testData$fat <- testY$fat

trainingData2 <- trainX2
trainingData2$fat <- trainY$fat

testData2 <- testX2
testData2$fat <- testY$fat



# ------------------------------------------------------------------------------
# Create folds explicitly and split train and test data
# ------------------------------------------------------------------------------
set.seed(100)

# create folds explicitly, default is 10 folds
indx <- createFolds(as.matrix(trainY), returnTrain = TRUE)

indx



# ------------------------------------------------------------------------------
# Set train control
# ------------------------------------------------------------------------------
ctrl <- trainControl(method = "cv", index = indx)



# ------------------------------------------------------------------------------
# Penelized models:  ridge regression
#   - Under standard assumptions, the coefficients produced by ordinary lest squares regression are unbiased and, of all unbiased linear techniques,
#     this model also has the lowest variance. However, given that the MSE is a combination of variance and bias, it is very possible to produce models
#     with smaller MSEs by allowing the parameter estimates to be biased.
#   - It is common that a small increase in bias can produce a substantial drop in the variance and hus a smaller MSE than ordinary least squares regression coefficients.
#     By adding the penalty to the sum of the squared errors, we are making a trade-off between the model variance and bias.
# ------------------------------------------------------------------------------
ridgeGrid <- expand.grid(lambda = seq(0, .1, length = 15))

set.seed(100)


# penalized models requires preProc (to make trade-off between the model variance and bias effective)
# Tune the penalty for ridge regression
ridgeTune <- train(x = trainX2, y = trainY$fat,
                   method = "ridge",
                   tuneGrid = ridgeGrid,
                   trControl = ctrl,
                   preProc = c("center", "scale"))

ridgeTune


print(update(plot(ridgeTune), xlab = "Penalty"))



# ------------------------------------------------------------------------------
# Penelized models:  lasso model (least absolute shrinkage and selection operator model)
# ------------------------------------------------------------------------------
enetGrid <- expand.grid(lambda = c(0, 0.01, .1), 
                        fraction = seq(.05, 1, length = 20))

set.seed(100)

enetTune <- train(x = trainX2, y = trainY$fat,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"))

enetTune


plot(enetTune)



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testResults_ridge <- data.frame(obs = testY$fat, ridge = predict(ridgeTune, testX2))
testResults_ridge <- testResults_ridge %>% mutate(resid = obs - ridge)

testResults_lasso <- data.frame(obs = testY$fat, lasso = predict(enetTune, testX2))
testResults_lasso <- testResults_lasso %>% mutate(resid = obs - lasso)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults_ridge$obs, testResults_ridge$ridge, testResults_lasso$lasso))

graphics.off()

par(mfrow = c(2,2))
with(testResults_ridge, plot(obs, ridge, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_ridge, plot(ridge, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testResults_lasso, plot(obs, lasso, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_lasso, plot(lasso, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

par(mfrow = c(2,2))
with(testResults_pls, plot(obs, PLS, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_pls, plot(PLS, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testResults_lasso, plot(obs, lasso, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_lasso, plot(lasso, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)




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
# Since the random number seed was initialized prior to running the ridge and lasso models, paired accuracy measurements exist for each data set.

resamp <- resamples(list(ridge = ridgeTune, lasso = enetTune))
resamp <- resamples(list(ridge = ridgeTune, lasso = enetTune, PLS = plsTune, PCR = pcrTune))

summary(resamp)


# --> 
# the mediand and mean of lasso is smaller than ridge


# ----------
# resampled distribution of metric
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2

trellis.par.set(theme1)
bwplot(resamp, layout = c(1, 3), auto.key = list(columns = 2))

trellis.par.set(caretTheme())
densityplot(resamp, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 2), adjust = 1.5)



# ----------
modelDifferences <- diff(resamp)
summary(modelDifferences)



# ----------
# The actual paired t-test:
# confidence interval for the difference of RMSE is -0.012 - 0.047, indicating that there is no evidence to support
# the idea that the RMSE for Lasso is better than that for Ridge
modelDifferences$statistics$RMSE

# -->
# p-values for the model comparisons are not small


# ----------
# resampled distribution of metric difference
trellis.par.set(theme1)
bwplot(modelDifferences, layout = c(1,3), auto.key = list(columns = 1))

trellis.par.set(caretTheme())
densityplot(modelDifferences, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 1))



# ------------------------------------------------------------------------------
# Extract model coefficient
# ------------------------------------------------------------------------------
# Ridge regression by enet function in the elasticnet package
ridgeModel <- elasticnet::enet(x = as.matrix(trainX2), y = trainY$fat, lambda = as.numeric(ridgeTune$bestTune))


# Lasso model
# the parameter lambda = 0 to fit lasso model
# the normalize argument will centered and scaled prior to modeling.
enetModel <- elasticnet::enet(x = as.matrix(trainX2), y = trainY$fat, lambda = 0, normalize = TRUE)


par(mfrow=c(1,2))
plot(ridgeModel, xvar = "fraction", use.color = TRUE)
plot(enetModel, xvar = "fraction", use.color = TRUE)



# ----------
# The predict function for enet objects generates predictions for one or more values of the lasso penalty simultaneously using the s and mode arguments.
# For ridge regression, we only desire a single lasso penalty of 0, so we want the full solution.
# To produce a ridge-regression solution we define s = 1 with mode = "fraction".
# This last option specifies how the amount of penalization is defined; in this case, a value of 1 corresponds to a fraction of 1, i.e.,the full solution.
ridgePred <- predict(ridgeModel, newx = as.matrix(testX2), s = 1, mode = "fraction", type = "fit")

enetPred <- predict(enetModel, newx = as.matrix(testX2), s = .1, mode = "fraction", type = "fit")



# ----------
# predicted value
ridgePred$fit
enetPred$fit



# ----------
# To determine which predictors are used in the model, the predict method is used with type = "coefficients"
ridgeCoef <- predict(ridgeModel, newx = as.matrix(testX2), s = 1, mode = "fraction", type = "coefficients")

enetCoef <- predict(enetModel, newx = as.matrix(testX2), s = .1, mode = "fraction", type = "coefficients")


ridgeCoef

enetCoef

