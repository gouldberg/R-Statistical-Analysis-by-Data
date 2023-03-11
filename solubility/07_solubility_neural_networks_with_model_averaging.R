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
# Nonlinear Regression Models:  Neural Networks
#  - Like partial least squares, the outcome is modeled by an intermediary set of unobserved variables. These hidden units are linear combinations of the 
#    original predictors, but, unlike PLS models, they are not estimated in a hierarchical fasion. This linear combination is typically transformed
#    by a nonlinear function, such as the logistic (i.e., sigmoidal) function.
#  - The parameters are usually initialized to random values are then specialized algorithms for solving the equations are used. The back-propagation algorithm
#    is a highly efficient methodology that works with derivative to find the optimal parameters.
#  - Very often, different locally optimal solutions can produce models that are very different but have nearly equivalent performance. This model instability can
#    sometimes hinder this model. As an alternative, several models can be created using different starting values and averaging the results of the these
#    model to produce a more stable prediction. Such model averaging often has a significantly positive effect on neural networks.
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(10)


# ----------
# Hyperparameter tuning:  weight decay (regularization) + hidden units
# bag:  use bagging instead of different random seeds.
nnetGrid <- expand.grid(decay = c(0, 0.01, .1),  size = c(1, 3, 5, 7, 9, 11, 13),  bag = FALSE)

nnetGrid



# ----------
# use transformed data (solTrainXtrans) and preProc("center", "scale") since we apply weight decay

# linout = TRUE:  for regression, the linear relationship between the hidden units and the prediction can be used with the option linout = TRUE
# trace = FALSE:  reduce the amount of printed output
# maxit: expand the number of iterations to find parameter estimates
# MaxNWts: the number of parameters used by the model
#  - For the solbility data, there are 228 predictors. A neural network model with 3 hidden units would estimate 3 * (228 + 1) + 3 + 1 = 691 parameters
#    while a model with 5 hidden units would have 1,151 coefficients. (single-layer-feed-forward network)

# method = "avNNet":  model averaging (default is 5 model averaging)


# THIS TAKES TIME !!!!: 1 hour + alpha
set.seed(100)

nnetTune <- train(x = solTrainXtrans, y = solTrainY,
                  method = "avNNet",
                  tuneGrid = nnetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"),
                  linout = TRUE,
                  trace = TRUE,
                  MaxNWts = 13 * (ncol(solTrainXtrans) + 1) + 13 + 1,
                  maxit = 1000,
                  allowParallel = TRUE)

nnetTune

nnetTune$finalModel


summary(nnetTune)



# ----------
# The cross-validated RMSE profiles:  Increasing the amount of weight decay clearly improved model performance,
# while more hidden units also reduce the model error.
# The optimal model used 11 hidden units with a total of 2,531 coefficients.
# The performance of the model is fairly stable for a high degree of regularization (lambda = 0.1), so smaller models could also be effective for these data.
plot(nnetTune)



# ------------------------------------------------------------------------------
# removing highly correlated predictors
# ------------------------------------------------------------------------------
# remove highly correlated predictors (to ensure that the maximum absolute correlation between the predictors is less than 0.75)
# remove 87 variables out of 228 variables
tooHigh <- findCorrelation(cor(solTrainXtrans), cutoff = 0.75)
length(tooHigh)

trainXnnet <- solTrainXtrans[, -tooHigh]

testXnnet <- solTestXtrans[, -tooHigh]



# ----------
nnetGrid2 <- expand.grid(.decay = c(0, 0.01, 0.1), .size = c(1:10), .bag = FALSE)

set.seed(100)


# IT TAKES TIME: 10-20 minutes
nnetTune2 <- train(x = trainXnnet, y = solTrainY,
                  method = "avNNet",
                  tuneGrid = nnetGrid2,
                  trControl = ctrl,
                  preProc = c("center", "scale"),
                  linout = TRUE,
                  trace = TRUE,
                  MaxNWts = 10 * (ncol(trainXnnet) + 1) + 10 + 1,
                  maxit = 500,
                  allowParallel = TRUE)

nnetTune2

nnetTune2$finalModel


# ----------
# The optimal model used 7 hidden units, decay = 0.1
plot(nnetTune2)



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testResults <- data.frame(obs = solTestY, NNet = predict(nnetTune, solTestXtrans))
testResults <- testResults %>% mutate(resid = obs - NNet)

testResults2 <- data.frame(obs = solTestY, NNet2 = predict(nnetTune2, testXnnet))
testResults2 <- testResults2 %>% mutate(resid = obs - NNet2)



# ----------
# diagnositc plot
# --> It seems that fit of NNet2 is better ...
axisRange <- extendrange(c(testResults$obs, testResults$NNet, testResults2$NNet2))

graphics.off()
par(mfrow = c(2,2))
with(testResults, plot(obs, NNet, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults, plot(NNet, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testResults2, plot(obs, NNet2, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults2, plot(NNet2, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



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

resamp <- resamples(list(NNet = nnetTune, NNet2 = nnetTune2))

summary(resamp)


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
# confidence interval for the difference of RMSE is -0.144 - -0.0748, indicating that there is evidence to support
# the idea that the RMSE for NNet is better than that for NNet2
modelDifferences$statistics$RMSE

# -->
# p-values for the model comparisons are small


# ----------
# resampled distribution of metric difference
trellis.par.set(theme1)
bwplot(modelDifferences, layout = c(1,3), auto.key = list(columns = 1))

trellis.par.set(caretTheme())
densityplot(modelDifferences, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 1))



# ------------------------------------------------------------------------------
# predict by avNNet
# ------------------------------------------------------------------------------
( best_size = nnetTune2$bestTune$size )

nnetAvg <- avNNet(x = trainXnnet, y = solTrainY,
                  size = best_size,
                  decay = nnetTune2$bestTune$decay,
                  repeats = 5,
                  linout = TRUE,
                  trace = TRUE,
                  maxit = 500,
                  MaxNWts =  best_size * (ncol(trainXnnet) + 1) + best_size + 1,
                  allowParallel = TRUE)

nnetAvg



# ----------
nnetAvgPred <- predict(nnetAvg, newx = as.matrix(trainXnnet))

nnetAvgPred


varImp(nnetAvg)
