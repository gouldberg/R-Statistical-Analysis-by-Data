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
# Linear Regression with 2 principal components without CV
# ------------------------------------------------------------------------------
lmFit2PC <- lm(fat ~ ., data = trainingData)

summary(lmFit2PC)



# variance inflation factors by decreasing order
sort(round(car::vif(lmFit2PC), digits = 3), decreasing=TRUE)



# ------------------------------------------------------------------------------
# Linear Regression with all predictors without CV
# ------------------------------------------------------------------------------
lmFitAllPredictors <- lm(fat ~ ., data = trainingData2)

summary(lmFitAllPredictors)



# variance inflation factors by decreasing order
sort(round(car::vif(lmFitAllPredictors), digits = 3), decreasing=TRUE)



# ------------------------------------------------------------------------------
# Linear Regression with 2 principal components or all predictors with CV(10)
# ------------------------------------------------------------------------------
set.seed(100)


# ----------
# with 2 principal components
lmTune0 <- train(fat ~ ., data = trainingData, method = "lm", trControl = ctrl)



# ----------
# with all predictors
lmTune02 <- train(fat ~ ., data = trainingData2, method = "lm", trControl = ctrl)



lmTune0                
lmTune02



# ----------
# Save the test set results in a data frame                 
testResults0 <- data.frame(obs = testY$fat, Linear_Regression = predict(lmTune0, testX))
testResults0 <- testResults0 %>% mutate(resid = obs - Linear_Regression)

testResults02 <- data.frame(obs = testY$fat, Linear_Regression = predict(lmTune02, testX2))
testResults02 <- testResults02 %>% mutate(resid = obs - Linear_Regression)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults0$obs, testResults0$Linear_Regression))

graphics.off()
par(mfrow = c(1,2))
with(testResults0, plot(obs, Linear_Regression, ylim = axisRange, xlim = axisRange, pch="*", col="red"))
abline(0, 1, col = "darkgrey", lty = 2)
with(testResults0, plot(Linear_Regression, resid, pch="*", col = "blue"))
abline(h = 0, col = "darkgrey", lty = 2)



axisRange <- extendrange(c(testResults02$obs, testResults02$Linear_Regression))

graphics.off()
par(mfrow = c(1,2))
with(testResults02, plot(obs, Linear_Regression, ylim = axisRange, xlim = axisRange, pch="*", col="red"))
abline(0, 1, col = "darkgrey", lty = 2)
with(testResults02, plot(Linear_Regression, resid, pch="*", col = "blue"))
abline(h = 0, col = "darkgrey", lty = 2)


