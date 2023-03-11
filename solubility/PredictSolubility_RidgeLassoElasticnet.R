# ------------------------------------------------------------------------------
# Predict solubility
#
# Ridge regression:  elasticnet package
# Lasso:  lars and elasticnet package
# Elasticnet:  elasticnet package
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/")

packages <- c("faraway", "dplyr", "Hmisc", "lattice", "ggplot2", "corrplot", "MASS", "lars", "caret", "AppliedPredictiveModeling", "elasticnet")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------
data(solubility)

allx <- rbind(solTrainX, solTestX)
ally <- c(solTrainY, solTestY)

train_row <- 1:nrow(solTrainX)
test_row <- (nrow(solTrainX)+1):nrow(allx)

trainingData <- solTrainXtrans
trainingData$solubility <- solTrainY
testData <- solTestXtrans
testData$solubility <- solTestY

# ------------------------------------------------------------------------------
# Basic analysis
# ------------------------------------------------------------------------------
allx %>% head()
str(allx)

histogram(ally, breaks=seq(-15,5,1))

# near zero variance variable
nzv <- nearZeroVar(allx)
colnames(allx)[nzv]
describe(allx[nzv])

# find variables with pairwise high correlation (absolute value)
corThresh <- 0.9
tooHigh <- findCorrelation(cor(allx), corThresh)
names(allx)[tooHigh]


# ------------------------------------------------------------------------------
# Box-Cox Transformation of variables -->  DO NOT USE THIS OUTPUT
# ------------------------------------------------------------------------------
allx_trans <- sapply(1:ncol(allx), function(x) {
  bc <- BoxCoxTrans(allx[,x] %>% unlist())
  bc2 <- predict(bc, allx[,x] %>% unlist())
})

colnames(allx_trans) <- colnames(allx)

all_trans <- data.frame(allx_trans, solbility=ally)


# ------------------------------------------------------------------------------
# FUNCTION to calculate RMSE
# ------------------------------------------------------------------------------
rmse <- function(x,y) sqrt(mean((y-x)^2))


# ------------------------------------------------------------------------------
# Normal Regression
# ------------------------------------------------------------------------------
modlm <- lm(solubility ~., data = trainingData)
summary(modlm)

stepmod <- step(modlm, direction = "both", k = 2)  #### IT TAKES LONG TIME !!!
stepmod$coefficients
xyplot(solTrainY ~ stepmod$fitted.values, type = c("p", "g"))
xyplot(stepmod$residuals ~ stepmod$fitted.values, type = c("p", "g"))


# RMSE train vs test  --> overfitting !!!
rmse(stepmod$fit, trainingData$solubility)
rmse(predict(stepmod, newdata = testData), testData$solubility)

# from caret packages
defaultSummary(data.frame(obs=solTrainY, pred=predict(stepmod, newdata=trainingData)))
defaultSummary(data.frame(obs=solTestY, pred=predict(stepmod, newdata=testData)))


# ------------------------------------------------------------------------------
# Ridge Regression by "enet" (elasticnet package)
# ------------------------------------------------------------------------------
# DOES NOT WORK -- lm.ridge (MASS package)
# rgmod <- lm.ridge(solubility ~ ., data = trainingData, lambda = seq(0, 5e-11, len=21))

# Tuning lambda by caret: method = "ridge"
ctrl <- trainControl(method = "cv", number = 10)
ridgeGrid <- data.frame(.lambda = seq(0, .1, length = 15))
set.seed(100)
rgmod_tune <- train(solTrainXtrans, solTrainY, method = "ridge", tuneGrid = ridgeGrid, trControl = ctrl, preProc = c("center", "scale"))

rgmod_tune
rgmod_tune$bestTune

# model by the tuned lambda
lambda <- rgmod_tune$bestTune %>% unlist() %>% unname()
rgmod <- enet(x = as.matrix(solTrainXtrans), y = solTrainY, lambda = lambda)
plot(rgmod)

# prediction:  s = 1 for ridge-regression
ypred <- predict(rgmod, newx = as.matrix(solTrainXtrans), s = 1, mode = "fraction", type="fit")
ypred_test <- predict(rgmod, newx = as.matrix(solTestXtrans), s = 1, mode = "fraction", type="fit")

defaultSummary(data.frame(obs=solTrainY, pred=ypred$fit))
defaultSummary(data.frame(obs=solTestY, pred=ypred_test$fit))



# ------------------------------------------------------------------------------
# Lasso by lars package:  138 variables are selected
# ------------------------------------------------------------------------------
set.seed(2018)
cvlmod <- cv.lars(as.matrix(trainingData %>% dplyr::select(-solubility)), trainingData$solubility, type="lasso", trace="TRUE")
min(cvlmod$cv)
cvlmod$index[which.min(cvlmod$cv)]


lmod <- lars(as.matrix(trainingData %>% dplyr::select(-solubility)), trainingData$solubility, type="lasso", trace="TRUE")
plot(lmod)

pred <- predict(lmod, s=cvlmod$index[which.min(cvlmod$cv)], type="coef", mode="fraction")
plot(pred$coefficients, type="h", ylab="Coefficients")
sum(pred$coef != 0)

pred <- predict(lmod, as.matrix(trainingData %>% dplyr::select(-solubility)), s=cvlmod$index[which.min(cvlmod$cv)], mode="fraction")
pred_test <- predict(lmod, as.matrix(testData %>% dplyr::select(-solubility)), s=cvlmod$index[which.min(cvlmod$cv)], mode="fraction")

defaultSummary(data.frame(obs=solTrainY, pred=pred$fit))
defaultSummary(data.frame(obs=solTestY, pred=pred_test$fit))


# ------------------------------------------------------------------------------
# Lasso by elasticnet package:  74 variables are selected
# ------------------------------------------------------------------------------
enetModel <- enet(x = as.matrix(solTrainXtrans), y = solTrainY, lambda = 0, normalize = TRUE)

enetCoef <- predict(enetModel, newx = as.matrix(solTrainXtrans), s = 0.1, mode = "fraction", type = "coefficients")
sum(enetCoef$coefficients != 0)

pred <- predict(enetModel, newx = as.matrix(solTrainXtrans), s = 0.1, mode = "fraction", type = "fit")
pred_test <- predict(enetModel, newx = as.matrix(solTestXtrans), s = 0.1, mode = "fraction", type = "fit")

defaultSummary(data.frame(obs=solTrainY, pred=pred$fit))
defaultSummary(data.frame(obs=solTestY, pred=pred_test$fit))


# ------------------------------------------------------------------------------
# Elasticnet by elasticnet package:  155 variables are selected
# ------------------------------------------------------------------------------
enetGrid <- expand.grid(.lambda = c(0, 0.001, 0.01, 0.1), .fraction = seq(0.05, 1, length = 20))

set.seed(100)
enetTune <- train(solTrainXtrans, solTrainY, method="enet", tuneGrid = enetGrid, trControl = ctrl, preProc = c("center", "scale"))
plot(enetTune)

enetTune$bestTune

tmp <- enetTune$bestTune %>% unlist() %>% unname()
fraction <- tmp[1]
lambda <- tmp[2]
enetModel <- enet(x = as.matrix(solTrainXtrans), y = solTrainY, lambda = tmp[2], normalize = TRUE)

enetCoef <- predict(enetModel, newx = as.matrix(solTrainXtrans), lambda = tmp[2], s = tmp[1], mode = "fraction", type = "coefficients")
sum(enetCoef$coefficients != 0)

pred <- predict(enetModel, newx = as.matrix(solTrainXtrans), lambda = tmp[2], s = tmp[1], mode = "fraction", type = "fit")
pred_test <- predict(enetModel, newx = as.matrix(solTestXtrans), lambda = tmp[2], s = tmp[1], mode = "fraction", type = "fit")

defaultSummary(data.frame(obs=solTrainY, pred=pred$fit))
defaultSummary(data.frame(obs=solTestY, pred=pred_test$fit))



