setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "MASS")
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
# Robust Linear Regression with all of the predictors without CV
#
#  - By default, MASS::rlm() employs Huber approach. The Huber function uses the squared residuals when they are "small" and 
#    the simple different between the observed and predicted values when the residuals are above a threshold (default is 2).
#  - This approach can effectivel minimize the influence of observations that fall away from the overall trend in the data.
# ------------------------------------------------------------------------------
trainingData <- solTrainXtrans
trainingData$Solubility <- solTrainY


rlmFitAllPredictors <- MASS::rlm(Solubility ~ ., data = trainingData)

summary(rlmFitAllPredictors)



# variance inflation factors by decreasing order
sort(round(car::vif(rlmFitAllPredictors), digits = 3), decreasing=TRUE)



# ------------------------------------------------------------------------------
# Robust Linear Regression with all of the predictors with CV(10)
# ------------------------------------------------------------------------------
set.seed(100)
rlmTune0 <- train(x = solTrainXtrans, y = solTrainY, method = "rlm", trControl = ctrl)

rlmTune0                 



# ----------
# Save the test set results in a data frame                 
testResults0 <- data.frame(obs = solTestY, RLR = predict(rlmTune0, solTestXtrans))
testResults0 <- testResults0 %>% mutate(resid = obs - RLR)


# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults0$obs, testResults0$Linear_Regression))

graphics.off()
par(mfrow = c(1,2))
with(testResults0, plot(obs, RLR, ylim = axisRange, xlim = axisRange, pch="*", col="red"))
abline(0, 1, col = "darkgrey", lty = 2)
with(testResults0, plot(RLR, resid, pch="*", col = "blue"))
abline(h = 0, col = "darkgrey", lty = 2)




