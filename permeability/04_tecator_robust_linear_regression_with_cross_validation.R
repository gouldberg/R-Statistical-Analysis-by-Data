rm(list=ls())
setwd("//media//kswada//MyFiles//R//tecator")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tecotor
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
# Robust Linear Regression with 2 principal components or all predictors without CV
#
#  - By default, MASS::rlm() employs Huber approach. The Huber function uses the squared residuals when they are "small" and 
#    the simple different between the observed and predicted values when the residuals are above a threshold (default is 2).
#  - This approach can effectivel minimize the influence of observations that fall away from the overall trend in the data.
# ------------------------------------------------------------------------------
rlmFit2PC <- MASS::rlm(fat ~ ., data = trainingData)
rlmFitAllPredictors <- MASS::rlm(fat ~ ., data = trainingData2)

summary(rlmFit2PC)
summary(rlmFitAllPredictors)



# variance inflation factors by decreasing order
sort(round(car::vif(rlmFitAllPredictors), digits = 3), decreasing=TRUE)



# ------------------------------------------------------------------------------
# Robust Linear Regression with 2 principal components or all predictors with CV(10)
# ------------------------------------------------------------------------------
set.seed(100)

rlmTune0 <- train(x = trainX, y = trainY$fat, method = "rlm", trControl = ctrl)
rlmTune02 <- train(x = trainX2, y = trainY$fat, method = "rlm", trControl = ctrl)


rlmTune0                 

rlmTune02



# ----------
# Save the test set results in a data frame          
testResults0 <- data.frame(obs = testY$fat, RLR = predict(rlmTune0, testX))
testResults0 <- testResults0 %>% mutate(resid = obs - RLR)


# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults0$obs, testResults0$RLR))

graphics.off()
par(mfrow = c(1,2))
with(testResults0, plot(obs, RLR, ylim = axisRange, xlim = axisRange, pch="*", col="red"))
abline(0, 1, col = "darkgrey", lty = 2)
with(testResults0, plot(RLR, resid, pch="*", col = "blue"))
abline(h = 0, col = "darkgrey", lty = 2)



# ----------
# Save the test set results in a data frame          
testResults02 <- data.frame(obs = testY$fat, RLR = predict(rlmTune02, testX2))
testResults02 <- testResults02 %>% mutate(resid = obs - RLR)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults02$obs, testResults0$RLR))

graphics.off()
par(mfrow = c(1,2))
with(testResults02, plot(obs, RLR, ylim = axisRange, xlim = axisRange, pch="*", col="red"))
abline(0, 1, col = "darkgrey", lty = 2)
with(testResults02, plot(RLR, resid, pch="*", col = "blue"))
abline(h = 0, col = "darkgrey", lty = 2)



