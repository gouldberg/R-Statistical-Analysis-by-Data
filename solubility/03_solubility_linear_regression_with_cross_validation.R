setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
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
# Linear Regression with all of the predictors without CV
# ------------------------------------------------------------------------------
trainingData <- solTrainXtrans
trainingData$Solubility <- solTrainY

lmFitAllPredictors <- lm(Solubility ~ ., data = trainingData)

summary(lmFitAllPredictors)



# variance inflation factors by decreasing order
sort(round(car::vif(lmFitAllPredictors), digits = 3), decreasing=TRUE)



# ------------------------------------------------------------------------------
# Linear Regression with all of the predictors with CV(10)
# ------------------------------------------------------------------------------
# This will produce some warnings that a 'rank-deficient fit may be misleading'. 
# This is related to the predictors being so highly correlated that some of the math has broken down.

set.seed(100)
lmTune0 <- train(x = solTrainXtrans, y = solTrainY, method = "lm", trControl = ctrl)

lmTune0                 


# ----------
# Save the test set results in a data frame                 
testResults0 <- data.frame(obs = solTestY, Linear_Regression = predict(lmTune0, solTestXtrans))
testResults0 <- testResults0 %>% mutate(resid = obs - Linear_Regression)


# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults0$obs, testResults0$Linear_Regression))

graphics.off()
par(mfrow = c(1,2))
with(testResults0, plot(obs, Linear_Regression, ylim = axisRange, xlim = axisRange, pch="*", col="red"))
abline(0, 1, col = "darkgrey", lty = 2)
with(testResults0, plot(Linear_Regression, resid, pch="*", col = "blue"))
abline(h = 0, col = "darkgrey", lty = 2)



# ------------------------------------------------------------------------------
# Linear Regression with removing extreme between-predictor correlation
# ------------------------------------------------------------------------------
# 38 predictors are identified and removed.
tooHigh <- findCorrelation(cor(solTrainXtrans), cutoff = .9)
length(tooHigh)

trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered  <-  solTestXtrans[, -tooHigh]



# ----------
set.seed(100)
lmTune <- train(x = trainXfiltered, y = solTrainY, method = "lm", trControl = ctrl)

lmTune



# ----------
# Save the test set results in a data frame                 
testResults <- data.frame(obs = solTestY, Linear_Regression = predict(lmTune, testXfiltered))
testResults <- testResults %>% mutate(resid = obs - Linear_Regression)


# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults$obs, testResults$Linear_Regression))

graphics.off()
par(mfrow = c(1,2))
with(testResults, plot(obs, Linear_Regression, ylim = axisRange, xlim = axisRange, pch="*", col="red"))
abline(0, 1, col = "darkgrey", lty = 2)
with(testResults, plot(Linear_Regression, resid, pch="*", col = "blue"))
abline(h = 0, col = "darkgrey", lty = 2)


# -->
# There does not appear to be any bias in the prediction, and the distribution between the predicted values and residuals
# appears to be random about zero.


