setwd("//media//kswada//MyFiles//R//credit_scoring")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Credit Scoring
# ------------------------------------------------------------------------------
data("GermanCredit", package = "caret")

data <- GermanCredit

dim(data)

str(data)

table(data$Class)



# ------------------------------------------------------------------------------
# remove variables
#
#  - First, remove near-zero variance predictors then get rid of a few predictors 
#    that duplicate values. For example, there are two possible values for the 
#    housing variable: "Rent", "Own" and "ForFree". So that we don't have linear
#    dependencies, we get rid of one of the levels (e.g. "ForFree")
# ------------------------------------------------------------------------------
data <- data[, -nearZeroVar(data)]


data$CheckingAccountStatus.lt.0 <- NULL
data$SavingsAccountBonds.lt.100 <- NULL
data$EmploymentDuration.lt.1 <- NULL
data$EmploymentDuration.Unemployed <- NULL
data$Personal.Male.Married.Widowed <- NULL
data$Property.Unknown <- NULL
data$Housing.ForFree <- NULL



# ------------------------------------------------------------------------------
# Split the data by Class into training (80%) and test sets (20%)
# ------------------------------------------------------------------------------
set.seed(100)

inTrain <- createDataPartition(data$Class, p = .8)[[1]]

dataTrain <- data[inTrain, ]

dataTest  <- data[-inTrain, ]



# ------------------------------------------------------------------------------
# Logistic regression with repeated cv
# ------------------------------------------------------------------------------
# We modeled "Class" by support vector machines (SVM) and logistic regression models.
# Since the performance of the two models were roughly equivalent, the logistic regression model was facored due to its simplicity.
set.seed(1056)

logisticReg <- train(Class ~ ., data = dataTrain, method = "glm",
                     trControl = trainControl(method = "repeatedcv", repeats = 5))

logisticReg



# ----------
# Predict the test set
creditResults <- data.frame(obs = dataTest$Class)
creditResults$lr_prob <- predict(logisticReg, dataTest, type = "prob")[, "Bad"]
creditResults$lr_pred <- predict(logisticReg, dataTest)
creditResults$Label <- ifelse(creditResults$obs == "Bad",  "True Outcome: Bad Credit",  "True Outcome: Good Credit")



# ------------------------------------------------------------------------------
# Support vector machines for comparison
# ------------------------------------------------------------------------------
# Using option tuneLength = 10, the cost values 2^-2 to 2^7 are evaluated
svm <- train(Class ~ ., data = dataTrain,
                 method = "svmRadial",
                 preProc = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE))


svm

svm$results


# ----------
# A line plot of the average performance.
plot(svm, scales = list(x = list(log = 2)))



# ----------
# Predict the test set
creditResults$svm_prob <- predict(svm, dataTest, type = "prob")[, "Bad"]
creditResults$svm_pred <- predict(svm, dataTest)



# ------------------------------------------------------------------------------
# Plot the probability of bad credit
# ------------------------------------------------------------------------------
# The probability of bad credit for the customers with good credit shows a skewed distribution where most customers' probabilities are quite low.
# In contrast, the probabilities for the customers with bad credit are flat (or uniformly distributed), reflecting the model's inability to distinguish bad credit cases
histogram( ~ lr_prob | Label, data = creditResults,
          layout = c(2, 1), nint = 20, xlab = "Probability of Bad Credit", type = "count")



# ----------
histogram( ~ svm_prob | Label, data = creditResults,
           layout = c(2, 1), nint = 20, xlab = "Probability of Bad Credit", type = "count")



# ------------------------------------------------------------------------------
# Calculate and plot the calibration curve
# ------------------------------------------------------------------------------
creditCalib <- calibration(obs ~ lr_prob, data = creditResults)
# creditCalib <- calibration(obs ~ svm_prob, data = creditResults)

creditCalib$data



# ----------
# The accuracy of the probability of bad credit degrades as it becomes larger to the point where no samples with bad credit were predicted with
# a probability above 82.7%
# THis pattern is indicative of a model that has both poor calibration and poor performance.
xyplot(creditCalib)



# ------------------------------------------------------------------------------
# Create the confusion matrix from the test set
# ------------------------------------------------------------------------------
# Sensitivity is around 0.40 ....
confusionMatrix(data = creditResults$lr_pred, reference = creditResults$obs)
confusionMatrix(data = creditResults$svm_pred, reference = creditResults$obs)



# ------------------------------------------------------------------------------
# ROC Curves
# ------------------------------------------------------------------------------
# Like glm(), roc() treats the last level of the factor as the event of interest so we use relevel() to change the observed class data
library(pROC)

creditROC_lr <- roc(relevel(creditResults$obs, "Good"), creditResults$lr_prob)
creditROC_svm <- roc(relevel(creditResults$obs, "Good"), creditResults$svm_prob)

coords(creditROC_lr, "all")[,1:3]
coords(creditROC_svm, "all")[,1:3]

auc(creditROC_lr)
auc(creditROC_svm)


ci(creditROC)



# ----------
# Note the x-axis is reversed
plot(creditROC_lr)


# Old-school
plot(creditROC_lr, legacy.axes = TRUE, col = "blue")
plot(creditROC_svm, legacy.axes = TRUE, add = TRUE, col = "red")



# ------------------------------------------------------------------------------
# Lift charts
# ------------------------------------------------------------------------------
creditLift_lr <- lift(obs ~ lr_prob, data = creditResults)
creditLift_svm <- lift(obs ~ svm_prob, data = creditResults)


xyplot(creditLift_lr)
xyplot(creditLift_svm)

