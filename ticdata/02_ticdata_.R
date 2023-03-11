# rm(list=ls())

setwd("//media//kswada//MyFiles//R//ticdata")

packages <- c("dplyr", "caret", "AppliedPredictiveModeling")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Functions for stats
# ------------------------------------------------------------------------------
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

fourStats <- function (data, lev = levels(data$obs), model = NULL){
  accKapp <- postResample(data[, "pred"], data[, "obs"])
  out <- c(accKapp, sensitivity(data[, "pred"], data[, "obs"], lev[1]), specificity(data[, "pred"], data[, "obs"], lev[2]))
  names(out)[3:4] <- c("Sens", "Spec")
  out
}



# ------------------------------------------------------------------------------
# Set training control
# ------------------------------------------------------------------------------
# two control functions are developed for situations when class probabilities can be created and when they cannot.
ctrl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = fiveStats)


ctrlNoProb <- ctrl
ctrlNoProb$summaryFunction <- fourStats
ctrlNoProb$classProbs <- FALSE



# ------------------------------------------------------------------------------
# Train by 
#   - Random Forest
#   - Flexible Discriminant Analysis Model (with MARS hinge functions)
#   - Logistic Regression
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(cores = 20)


# ----------
# Random Forest
set.seed(1410)

rfFit <- train(CARAVAN ~ ., data = trainingInd, method = "rf", trControl = ctrl, ntree = 1500, tuneLength = 5, metric = "ROC")

rfFit

plot(rfFit)



# ----------
# logistic regression with noNZVset
set.seed(1410)

lrFit <- train(CARAVAN ~ ., data = trainingInd[, noNZVSet], method = "glm", trControl = ctrl, metric = "ROC")

lrFit

plot(lrFit)



# -----------
# flexible discriminant analysis
set.seed(1401)

fdaFit <- train(CARAVAN ~ ., data = training, method = "fda", tuneGrid = data.frame(degree = 1, nprune = 1:25), metric = "ROC", trControl = ctrl)

fdaFit

plot(fdaFit)



# ----------
# confusion matrix for evaluation data
confusionMatrix(predict(rfFit, evaluationInd), evalResults$CARAVAN)
confusionMatrix(predict(lrFit, evaluationInd), evalResults$CARAVAN)
confusionMatrix(predict(fdaFit, evaluationInd), evalResults$CARAVAN)



# ----------
# prediction for evaluation and test data set
evalResults$RF <- predict(rfFit, evaluationInd, type = "prob")[,1]
testResults$RF <- predict(rfFit, testingInd, type = "prob")[,1]

valResults$LogReg <- predict(lrFit, evaluationInd[, noNZVSet], type = "prob")[,1]
testResults$LogReg <- predict(lrFit, testingInd[, noNZVSet], type = "prob")[,1]

evalResults$FDA <- predict(fdaFit, evaluation[, predictors], type = "prob")[,1]
testResults$FDA <- predict(fdaFit, testing[, predictors], type = "prob")[,1]



# ----------
# ROC
rfROC <- roc(evalResults$CARAVAN, evalResults$RF, levels = rev(levels(evalResults$CARAVAN)))
lrROC <- roc(evalResults$CARAVAN, evalResults$LogReg, levels = rev(levels(evalResults$CARAVAN)))
fdaROC <- roc(evalResults$CARAVAN, evalResults$FDA, levels = rev(levels(evalResults$CARAVAN)))


plotTheme <- caretTheme()
plot(fdaROC, type = "S", col = plotTheme$superpose.line$col[3], legacy.axes = TRUE)
plot(rfROC, type = "S", col = plotTheme$superpose.line$col[1], add = TRUE, legacy.axes = TRUE)
plot(lrROC, type = "S", col = plotTheme$superpose.line$col[2], add = TRUE, legacy.axes = TRUE)

legend(.7, .25, c("Random Forest", "Logistic Regression", "FDA (MARS)"), cex = .85, col = plotTheme$superpose.line$col[1:3], lwd = rep(2, 3), lty = rep(1, 3))


labs <- c(RF = "Random Forest", LogReg = "Logistic Regression", FDA = "FDA (MARS)")
lift1 <- lift(CARAVAN ~ RF + LogReg + FDA, data = evalResults, labels = labs)
xyplot(lift1, ylab = "%Events Found", xlab =  "%Customers Evaluated", lwd = 2, type = "l")


auc(rfROC);  auc(lrROC);  auc(fdaROC);




################################################################################
### Section 16.4 Alternate Cutoffs

rfThresh <- coords(rfROC, x = "best", ret="threshold",
                   best.method="closest.topleft")
rfThreshY <- coords(rfROC, x = "best", ret="threshold",
                    best.method="youden")

cutText <- ifelse(rfThresh == rfThreshY,
                  "is the same as",
                  "is similar to")

evalResults$rfAlt <- factor(ifelse(evalResults$RF > rfThresh,
                                   "insurance", "noinsurance"),
                            levels = levels(evalResults$CARAVAN))
testResults$rfAlt <- factor(ifelse(testResults$RF > rfThresh,
                                   "insurance", "noinsurance"),
                            levels = levels(testResults$CARAVAN))
rfAltEvalCM <- confusionMatrix(evalResults$rfAlt, evalResults$CARAVAN)
rfAltEvalCM

rfAltTestCM <- confusionMatrix(testResults$rfAlt, testResults$CARAVAN)
rfAltTestCM

rfTestCM <- confusionMatrix(predict(rfFit, testingInd), testResults$CARAVAN)


plot(rfROC, print.thres = c(.5, .3, .10, rfThresh), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, legacy.axes = TRUE)

################################################################################
### Section 16.5 Adjusting Prior Probabilities

priors <- table(ticdata$CARAVAN)/nrow(ticdata)*100
fdaPriors <- fdaFit
fdaPriors$finalModel$prior <- c(insurance = .6, noinsurance =  .4)
fdaPriorPred <- predict(fdaPriors, evaluation[,predictors])
evalResults$FDAprior <-  predict(fdaPriors, evaluation[,predictors], type = "prob")[,1]
testResults$FDAprior <-  predict(fdaPriors, testing[,predictors], type = "prob")[,1]
fdaPriorCM <- confusionMatrix(fdaPriorPred, evaluation$CARAVAN)
fdaPriorCM

fdaPriorROC <- roc(testResults$CARAVAN, testResults$FDAprior,
                   levels = rev(levels(testResults$CARAVAN)))
fdaPriorROC

################################################################################
### Section 16.7 Sampling Methods

set.seed(1237)
downSampled <- downSample(trainingInd[, -ncol(trainingInd)], training$CARAVAN)

set.seed(1237)
upSampled <- upSample(trainingInd[, -ncol(trainingInd)], training$CARAVAN)

library(DMwR)
set.seed(1237)
smoted <- SMOTE(CARAVAN ~ ., data = trainingInd)

set.seed(1410)
rfDown <- train(Class ~ ., data = downSampled,
                "rf",
                trControl = ctrl,
                ntree = 1500,
                tuneLength = 5,
                metric = "ROC")
rfDown

evalResults$RFdown <- predict(rfDown, evaluationInd, type = "prob")[,1]
testResults$RFdown <- predict(rfDown, testingInd, type = "prob")[,1]
rfDownROC <- roc(evalResults$CARAVAN, evalResults$RFdown,
                 levels = rev(levels(evalResults$CARAVAN)))
rfDownROC

set.seed(1401)
rfDownInt <- train(CARAVAN ~ ., data = trainingInd,
                   "rf",
                   ntree = 1500,
                   tuneLength = 5,
                   strata = training$CARAVAN,
                   sampsize = rep(sum(training$CARAVAN == "insurance"), 2),
                   metric = "ROC",
                   trControl = ctrl)
rfDownInt

evalResults$RFdownInt <- predict(rfDownInt, evaluationInd, type = "prob")[,1]
testResults$RFdownInt <- predict(rfDownInt, testingInd, type = "prob")[,1]
rfDownIntRoc <- roc(evalResults$CARAVAN,
                    evalResults$RFdownInt,
                    levels = rev(levels(training$CARAVAN)))
rfDownIntRoc

set.seed(1410)
rfUp <- train(Class ~ ., data = upSampled,
              "rf",
              trControl = ctrl,
              ntree = 1500,
              tuneLength = 5,
              metric = "ROC")
rfUp

evalResults$RFup <- predict(rfUp, evaluationInd, type = "prob")[,1]
testResults$RFup <- predict(rfUp, testingInd, type = "prob")[,1]
rfUpROC <- roc(evalResults$CARAVAN, evalResults$RFup,
               levels = rev(levels(evalResults$CARAVAN)))
rfUpROC

set.seed(1410)
rfSmote <- train(CARAVAN ~ ., data = smoted,
                 "rf",
                 trControl = ctrl,
                 ntree = 1500,
                 tuneLength = 5,
                 metric = "ROC")
rfSmote

evalResults$RFsmote <- predict(rfSmote, evaluationInd, type = "prob")[,1]
testResults$RFsmote <- predict(rfSmote, testingInd, type = "prob")[,1]
rfSmoteROC <- roc(evalResults$CARAVAN, evalResults$RFsmote,
                  levels = rev(levels(evalResults$CARAVAN)))
rfSmoteROC

rfSmoteCM <- confusionMatrix(predict(rfSmote, evaluationInd), evalResults$CARAVAN)
rfSmoteCM

samplingSummary <- function(x, evl, tst)
{
  lvl <- rev(levels(tst$CARAVAN))
  evlROC <- roc(evl$CARAVAN,
                predict(x, evl, type = "prob")[,1],
                levels = lvl)
  rocs <- c(auc(evlROC),
            auc(roc(tst$CARAVAN,
                    predict(x, tst, type = "prob")[,1],
                    levels = lvl)))
  cut <- coords(evlROC, x = "best", ret="threshold",
                best.method="closest.topleft")
  bestVals <- coords(evlROC, cut, ret=c("sensitivity", "specificity"))
  out <- c(rocs, bestVals*100)
  names(out) <- c("evROC", "tsROC", "tsSens", "tsSpec")
  out
  
}

rfResults <- rbind(samplingSummary(rfFit, evaluationInd, testingInd),
                   samplingSummary(rfDown, evaluationInd, testingInd),
                   samplingSummary(rfDownInt, evaluationInd, testingInd),
                   samplingSummary(rfUp, evaluationInd, testingInd),
                   samplingSummary(rfSmote, evaluationInd, testingInd))
rownames(rfResults) <- c("Original", "Down--Sampling",  "Down--Sampling (Internal)",
                         "Up--Sampling", "SMOTE")

rfResults

rocCols <- c("black", rgb(1, 0, 0, .5), rgb(0, 0, 1, .5))

plot(roc(testResults$CARAVAN, testResults$RF, levels = rev(levels(testResults$CARAVAN))),
     type = "S", col = rocCols[1], legacy.axes = TRUE)
plot(roc(testResults$CARAVAN, testResults$RFdownInt, levels = rev(levels(testResults$CARAVAN))),
     type = "S", col = rocCols[2],add = TRUE, legacy.axes = TRUE)
plot(roc(testResults$CARAVAN, testResults$RFsmote, levels = rev(levels(testResults$CARAVAN))),
     type = "S", col = rocCols[3], add = TRUE, legacy.axes = TRUE)
legend(.6, .4,
       c("Normal", "Down-Sampling (Internal)", "SMOTE"),
       lty = rep(1, 3),
       lwd = rep(2, 3),
       cex = .8,
       col = rocCols)

xyplot(lift(CARAVAN ~ RF + RFdownInt + RFsmote,
            data = testResults),
       type = "l",
       ylab = "%Events Found",
       xlab =  "%Customers Evaluated")


################################################################################
### Section 16.8 Cost–Sensitive Training

library(kernlab)

set.seed(1157)
sigma <- sigest(CARAVAN ~ ., data = trainingInd[, noNZVSet], frac = .75)
names(sigma) <- NULL

svmGrid1 <- data.frame(sigma = sigma[2],
                       C = 2^c(2:10))

set.seed(1401)
svmFit <- train(CARAVAN ~ .,
                data = trainingInd[, noNZVSet],
                method = "svmRadial",
                tuneGrid = svmGrid1,
                preProc = c("center", "scale"),
                metric = "Kappa",
                trControl = ctrl)
svmFit

evalResults$SVM <- predict(svmFit, evaluationInd[, noNZVSet], type = "prob")[,1]
testResults$SVM <- predict(svmFit, testingInd[, noNZVSet], type = "prob")[,1]
svmROC <- roc(evalResults$CARAVAN, evalResults$SVM,
              levels = rev(levels(evalResults$CARAVAN)))
svmROC

svmTestROC <- roc(testResults$CARAVAN, testResults$SVM,
                  levels = rev(levels(testResults$CARAVAN)))
svmTestROC

confusionMatrix(predict(svmFit, evaluationInd[, noNZVSet]), evalResults$CARAVAN)

confusionMatrix(predict(svmFit, testingInd[, noNZVSet]), testingInd$CARAVAN)


set.seed(1401)
svmWtFit <- train(CARAVAN ~ .,
                  data = trainingInd[, noNZVSet],
                  method = "svmRadial",
                  tuneGrid = svmGrid1,
                  preProc = c("center", "scale"),
                  metric = "Kappa",
                  class.weights = c(insurance = 18, noinsurance = 1),
                  trControl = ctrlNoProb)
svmWtFit

svmWtEvalCM <- confusionMatrix(predict(svmWtFit, evaluationInd[, noNZVSet]), evalResults$CARAVAN)
svmWtEvalCM

svmWtTestCM <- confusionMatrix(predict(svmWtFit, testingInd[, noNZVSet]), testingInd$CARAVAN)
svmWtTestCM


initialRpart <- rpart(CARAVAN ~ ., data = training,
                      control = rpart.control(cp = 0.0001))
rpartGrid <- data.frame(cp = initialRpart$cptable[, "CP"])

cmat <- list(loss = matrix(c(0, 1, 20, 0), ncol = 2))
set.seed(1401)
cartWMod <- train(x = training[,predictors],
                  y = training$CARAVAN,
                  method = "rpart",
                  trControl = ctrlNoProb,
                  tuneGrid = rpartGrid,
                  metric = "Kappa",
                  parms = cmat)
cartWMod


library(C50)
c5Grid <- expand.grid(model = c("tree", "rules"),
                      trials = c(1, (1:10)*10),
                      winnow = FALSE)

finalCost <- matrix(c(0, 20, 1, 0), ncol = 2)
rownames(finalCost) <- colnames(finalCost) <- levels(training$CARAVAN)
set.seed(1401)
C5CostFit <- train(training[, predictors],
                   training$CARAVAN,
                   method = "C5.0",
                   metric = "Kappa",
                   tuneGrid = c5Grid,
                   cost = finalCost,
                   control = C5.0Control(earlyStopping = FALSE),
                   trControl = ctrlNoProb)

C5CostCM <- confusionMatrix(predict(C5CostFit, testing), testing$CARAVAN)
C5CostCM


################################################################################
### Session Information

sessionInfo()

q("no")
