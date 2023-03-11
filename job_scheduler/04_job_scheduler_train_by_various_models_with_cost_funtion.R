setwd("//media//kswada//MyFiles//R//job_scheduler")

packages <- c("dplyr", "caret", "AppliedPredictiveModeling", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Job Scheduler
# ------------------------------------------------------------------------------
data(schedulingData)


str(schedulingData)



# ------------------------------------------------------------------------------
# Create the cost matrix
# ------------------------------------------------------------------------------
# Longer jobs that are misclassified as fast are penalized more in thie criterion
costMatrix <- ifelse(diag(4) == 1, 0, 1)
costMatrix[4, 1] <- 10
costMatrix[3, 1] <- 5
costMatrix[4, 2] <- 5
costMatrix[3, 2] <- 5
rownames(costMatrix) <- colnames(costMatrix) <- levels(trainData$Class)

costMatrix



# ------------------------------------------------------------------------------
# Cost function and cost summary
# ------------------------------------------------------------------------------
cost <- function(pred, obs)
{
  isNA <- is.na(pred)
  if(!all(isNA))
  {
    pred <- pred[!isNA]
    obs <- obs[!isNA]
    
    cost <- ifelse(pred == obs, 0, 1)
    if(any(pred == "VF" & obs == "L")) cost[pred == "L" & obs == "VF"] <- 10
    if(any(pred == "F" & obs == "L")) cost[pred == "F" & obs == "L"] <- 5
    if(any(pred == "F" & obs == "M")) cost[pred == "F" & obs == "M"] <- 5
    if(any(pred == "VF" & obs == "M")) cost[pred == "VF" & obs == "M"] <- 5
    out <- mean(cost)
  } else out <- NA
  out
}



# Make a summary function that can be used with caret's train() function
costSummary <- function (data, lev = NULL, model = NULL)
{
  if (is.character(data$obs))  data$obs <- factor(data$obs, levels = lev)
  c(postResample(data[, "pred"], data[, "obs"]),
    Cost = cost(data[, "pred"], data[, "obs"]))
}




# ------------------------------------------------------------------------------
# Create a control object for the models
# ------------------------------------------------------------------------------
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5,
                     summaryFunction = costSummary)



# ------------------------------------------------------------------------------
# Training
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(20)



# ----------
# CART
set.seed(857)
rpFit <- train(x = trainData[, predictors], y = trainData$Class, method = "rpart", metric = "Cost",
               maximize = FALSE, tuneLength = 20, trControl = ctrl)

set.seed(857)
rpFitCost <- train(x = trainData[, predictors], y = trainData$Class, method = "rpart", metric = "Cost",
                   maximize = FALSE, tuneLength = 20, parms =list(loss = costMatrix), trControl = ctrl)

plot(rpFit)
plot(rpFitCost)



# ----------
# Linear Discriminant Analysis
set.seed(857)
ldaFit <- train(x = expandedTrain, y = trainData$Class, method = "lda", metric = "Cost",
                maximize = FALSE, trControl = ctrl)


# sparse LDA (penalized LDA)
sldaGrid <- expand.grid(NumVars = seq(2, 112, by = 5), lambda = c(0, 0.01, .1, 1, 10))

set.seed(857)
sldaFit <- train(x = expandedTrain, y = trainData$Class, method = "sparseLDA", tuneGrid = sldaGrid,
                 preProc = c("center", "scale"), metric = "Cost", maximize = FALSE, trControl = ctrl)

plot(sldaFit)



# ----------
# neural networks
nnetGrid <- expand.grid(decay = c(0, 0.001, 0.01, .1, .5), size = (1:10)*2 - 1)

set.seed(857)
nnetFit <- train(modForm,  data = trainData, method = "nnet", metric = "Cost",
                 maximize = FALSE, tuneGrid = nnetGrid, trace = FALSE, MaxNWts = 2000, maxit = 1000,
                 preProc = c("center", "scale"), trControl = ctrl)


plot(nnetFit)



# ----------
# partial least squares
set.seed(857)
plsFit <- train(x = expandedTrain, y = trainData$Class, method = "pls", metric = "Cost",
                maximize = FALSE, tuneLength = 100, preProc = c("center", "scale"), trControl = ctrl)


plot(plsFit)



# ----------
# flexible discriminan analysis
set.seed(857)
fdaFit <- train(modForm, data = trainData, method = "fda", metric = "Cost",
                maximize = FALSE, tuneLength = 25, trControl = ctrl)


plot(fdaFit)



# ----------
# random forest
set.seed(857)
rfFit <- train(x = trainData[, predictors], y = trainData$Class, method = "rf", metric = "Cost",
               maximize = FALSE, tuneLength = 10, ntree = 2000, importance = TRUE, trControl = ctrl)


set.seed(857)
rfFitCost <- train(x = trainData[, predictors], y = trainData$Class, method = "rf", metric = "Cost",
                   maximize = FALSE, tuneLength = 10, ntree = 2000,
                   classwt = c(VF = 1, F = 1, M = 5, L = 10), importance = TRUE, trControl = ctrl)


plot(rfFit)
plot(rfFitCost)



# ----------
# C5.0
c5Grid <- expand.grid(trials = c(1, (1:10)*10), model = "tree", winnow = c(TRUE, FALSE))

set.seed(857)
c50Fit <- train(x = trainData[, predictors], y = trainData$Class, method = "C5.0", metric = "Cost",
                maximize = FALSE, tuneGrid = c5Grid, trControl = ctrl)


set.seed(857)
c50Cost <- train(x = trainData[, predictors], y = trainData$Class, method = "C5.0", metric = "Cost",
                 maximize = FALSE, costs = costMatrix, tuneGrid = c5Grid, trControl = ctrl)


plot(c50Fit)
plot(c50Cost)



# ----------
# bagged trees
set.seed(857)
bagFit <- train(x = trainData[, predictors], y = trainData$Class, method = "treebag", metric = "Cost",
                maximize = FALSE, nbagg = 50, trControl = ctrl)



# ----------
# Use the caret bag() function to bag the cost-sensitive CART model
rpCost <- function(x, y){
  costMatrix <- ifelse(diag(4) == 1, 0, 1)
  costMatrix[4, 1] <- 10
  costMatrix[3, 1] <- 5
  costMatrix[4, 2] <- 5
  costMatrix[3, 2] <- 5
  library(rpart)
  tmp <- x
  tmp$y <- y
  rpart(y~., data = tmp, control = rpart.control(cp = 0), parms =list(loss = costMatrix))
}

rpPredict <- function(object, x) predict(object, x)

rpAgg <- function (x, type = "class"){
  pooled <- x[[1]] * NA
  n <- nrow(pooled)
  classes <- colnames(pooled)
  for (i in 1:ncol(pooled)){
    tmp <- lapply(x, function(y, col) y[, col], col = i)
    tmp <- do.call("rbind", tmp)
    pooled[, i] <- apply(tmp, 2, median)
  }
  pooled <- apply(pooled, 1, function(x) x/sum(x))
  if (n != nrow(pooled)) pooled <- t(pooled)
  out <- factor(classes[apply(pooled, 1, which.max)], levels = classes)
  out
}


set.seed(857)
rpCostBag <- train(trainData[, predictors], trainData$Class, "bag", B = 50,
                   bagControl = bagControl(fit = rpCost, predict = rpPredict, aggregate = rpAgg, downSample = FALSE, allowParallel = TRUE),
                   trControl = ctrl)



# ----------
# SVM
set.seed(857)
svmRFit <- train(modForm, data = trainData, method = "svmRadial", metric = "Cost",
                 maximize = FALSE, preProc = c("center", "scale"), tuneLength = 15, trControl = ctrl)


set.seed(857)
svmRFitCost <- train(modForm, data = trainData, method = "svmRadial", metric = "Cost",
                     maximize = FALSE, preProc = c("center", "scale"), class.weights = c(VF = 1, F = 1, M = 5, L = 10),
                     tuneLength = 15, trControl = ctrl)


plot(svmRFit)
plot(svmRFitCost)



# ------------------------------------------------------------------------------
# model list
# ------------------------------------------------------------------------------
modelList <- list(C50 = c50Fit, C50Cost = c50Cost,
                  CART = rpFit, CARTCost = rpFitCost, BaggingCost = rpCostBag,
                  FDA = fdaFit, 
                  SVM = svmRFit, SVMWeights = svmRFitCost,
                  PLS = plsFit, 
                  RF = rfFit, RFCost = rfFitCost,
                  LDA = ldaFit, LDASparse = sldaFit,
                  NNET = nnetFit, Bagging = bagFit)



# ------------------------------------------------------------------------------
# compare model performance (for cross-validation training data)
# ------------------------------------------------------------------------------
# Compare model performance for cross-validation (trainig data)
resamp <- resamples(modelList)
summary(resamp)


# resampled distribution of metric: cost
plot(bwplot(resamp, metric = "Cost"))



# -->
# The linear models, such as LDA and PLS, did not do well here.
# Feature selection did not help the linear discriminant model, but this may be due to that model's inability to handle nonlinear class boundaries
# FDA also showed poor performance in terms of cost

# There is a cluster of models with average costs that are likely to be equivalent, mostly SVMs and the various tree ensemble methods.
# Using costs/weights had significant positive effects on the single CART tree and SVMs.



# ----------
# resampled distribution of metric: Accuracy, Cost, Kappa
trellis.par.set(theme1)
par(mfrow=c(1,1))
bwplot(resamp, auto.key = list(columns = 15))



# -->
# CART (costs) and SVM (weights) did do well in terms of "Cost" metric, but worst in simple Accuracy and Kappa.


trellis.par.set(caretTheme())
densityplot(resamp, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 2), adjust = 1.5)



# ----------
modelDifferences <- diff(resamp)
summary(modelDifferences)


# The actual paired t-test:
names(modelDifferences$statistics$Cost)
modelDifferences$statistics$Cost$`C50.diff.RF`


# -->
# There is no evidence that there is performance difference (in terms Cost) between Random Forest and C5.0



# ----------
# Compare cost-senstive CART and Random Forest
confusionMatrix(rpFitCost, "none")
confusionMatrix(rfFit, "none") 


# -->
# For Random Forest, the number of long jobs that were misclassified as very fast was 10 while the same value for the cost-sensitive CART.
# The CART shows very poor accuracy for the fast jobs compared to the random forest model
# However, the opposite is true for moderately long jobs.



# ------------------------------------------------------------------------------
# Predictor Importance
# ------------------------------------------------------------------------------
plot(varImp(c50Cost, scale = FALSE), top=7, scales = list(y = list(cex = .95)))

plot(varImp(rfFit, scale = FALSE), top=7, scales = list(y = list(cex = .95)))

plot(varImp(rfFitCost, scale = FALSE), top=7, scales = list(y = list(cex = .95)))



# ------------------------------------------------------------------------------
# prediction for test data
# ------------------------------------------------------------------------------
nrow(testData)

c50_pred_prob <- predict(c50Fit, newdata = testData, type ="prob")
c50_pred <- predict(c50Fit, newdata = testData, type ="raw")

# c50Cost_pred_prob <- predict(c50Cost, newdata = testData, type ="prob")
c50Cost_pred <- predict(c50Cost, newdata = testData, type ="raw")

rf_pred_prob <- predict(rfFit, newdata = testData, type ="prob")
rf_pred <- predict(rfFit, newdata = testData, type ="raw")

bag_pred_prob <- predict(bagFit, newdata = testData, type ="prob")
bag_pred <- predict(bagFit, newdata = testData, type ="raw")

rfCost_pred_prob <- predict(rfFitCost, newdata = testData, type ="prob")
rfCost_pred <- predict(rfFitCost, newdata = testData, type ="raw")

rpCost_pred_prob <- predict(rpFitCost, newdata = testData, type ="prob")
rpCost_pred <- predict(rpFitCost, newdata = testData, type ="raw")

# svmRCost_pred_prob <- predict(svmRFitCost, newdata = testData, type ="prob")
svmRCost_pred <- predict(svmRFitCost, newdata = testData, type ="raw")

# svmR_pred_prob <- predict(svmRFit, newdata = testData, type ="prob")
svmR_pred <- predict(svmRFit, newdata = testData, type ="raw")



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_c50 <- data.frame(obs = testData$Class, c50_pred_prob_VF = c50_pred_prob$VF, c50_pred_prob_F = c50_pred_prob$F, c50_pred_prob_M = c50_pred_prob$M, c50_pred_prob_L = c50_pred_prob$L, c50_pred = c50_pred)
testRes_rf <- data.frame(obs = testData$Class, rf_pred_prob_VF = rf_pred_prob$VF, rf_pred_prob_F = rf_pred_prob$F, rf_pred_prob_M = rf_pred_prob$M, rf_pred_prob_L = rf_pred_prob$L, rf_pred = rf_pred)
testRes_rfCost <- data.frame(obs = testData$Class, rfCost_pred_prob_VF = rfCost_pred_prob$VF, rfCost_pred_prob_F = rfCost_pred_prob$F, rfCost_pred_prob_M = rfCost_pred_prob$M, rfCost_pred_prob_L = rfCost_pred_prob$L, rfCost_pred = rfCost_pred)
testRes_rpCost <- data.frame(obs = testData$Class, rpCost_pred_prob_VF = rpCost_pred_prob$VF, rpCost_pred_prob_F = rpCost_pred_prob$F, rpCost_pred_prob_M = rpCost_pred_prob$M, rpCost_pred_prob_L = rpCost_pred_prob$L, rpCost_pred = rpCost_pred)
testRes_bag <- data.frame(obs = testData$Class, bag_pred_prob_VF = bag_pred_prob$VF, bag_pred_prob_F = bag_pred_prob$F, bag_pred_prob_M = bag_pred_prob$M, bag_pred_prob_L = bag_pred_prob$L, bag_pred = bag_pred)



# ----------
# confusion matrix (by manual)
addmargins(xtabs(~ obs + c50_pred, data = testRes_c50))

addmargins(xtabs(~ obs + rfCost_pred, data = testRes_rfCost))
addmargins(xtabs(~ obs + rf_pred, data = testRes_rf))
addmargins(xtabs(~ obs + rpCost_pred, data = testRes_rpCost))

addmargins(xtabs(~ obs + bag_pred, data = testRes_bag))



# ----------
# confusion matrix
confusionMatrix(c50_pred, testData$Class)

confusionMatrix(rfCost_pred, testData$Class)
confusionMatrix(rf_pred, testData$Class)
confusionMatrix(rpCost_pred, testData$Class)

confusionMatrix(bag_pred, testData$Class)



# ----------
# Plot the probability of each class
histogram( ~ c50_pred_prob_VF | obs, data = testRes_c50,
           layout = c(2, 2), nint = 20, xlab = "Probability of VF", type = "count")

histogram( ~ c50_pred_prob_F | obs, data = testRes_c50,
           layout = c(2, 2), nint = 20, xlab = "Probability of F", type = "count")

histogram( ~ c50_pred_prob_M | obs, data = testRes_c50,
           layout = c(2, 2), nint = 20, xlab = "Probability of M", type = "count")

histogram( ~ c50_pred_prob_L | obs, data = testRes_c50,
           layout = c(2, 2), nint = 20, xlab = "Probability of L", type = "count")
