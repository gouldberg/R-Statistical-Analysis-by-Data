# Choosing betweeen models
#   - Start with the least interpretable and most flexible model, here support verctor machine.
#   - Investigate simpler models that are less opaque, such as multivariate adaptive regression splines (MARS),
#     partial least squares, generalized additive models, or naive Bayes model  --> here logistic regression


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

data_train <- data[inTrain, ]

data_test  <- data[-inTrain, ]


nrow(data_train)
nrow(data_test)



# ------------------------------------------------------------------------------
# Hyperparameter estimation for the Gaussian Radial Basis Kernel
#
#  - Radial Basis Function (RBF) kernel function has an additional tuning parameter associated with it denoted as sigma, 
#    which impacts the smoothness of the decision boundary.
#    Caputo et.al. (2002) describes an analytical formula that can be used to get reasonable estimates of sigma.
#    The caret function train uses this approach to estimate the kernel parameter leaving only the cost paramter for tuning.
#
#  - Given a range of values for the "sigma" inverse width parameter in the Gaussian Radial Basis kernel for use with Support Vector Machines.
#    The estimation is based on the data to be used.
#  - kernlab::sigest() estimates the range of values for the sigma parameter which would return good results when used with a 
#    Support Vector machine (ksvm).  The estimation is based upon the 0.1 and 0.9 quantile of ...
#    Basically any value in between those two bounds will produce good results.
#    kernlab::sigest() returns a vector of length 3 defining the range (0.1 quantile, median and 0.9 quantile) of the sigma hyperparameter.
# 
# This generally is not needed, but was used here so that we could trim the cost values to a presentable range
# and to re-use later with different resampling methods.
# ------------------------------------------------------------------------------

library(kernlab)

set.seed(231)


# ----------
# Hyperparameter estimation for the Gaussian Radial Basis Kernel by sigest()
# frac is fraction of data to use for estimation. By default a quarter of the data is used to estimate the range of the sigma hyperparameter.

sigDist <- sigest(Class ~ ., data = data_train, frac = 1)
sigDist



# ------------------------------------------------------------------------------
# Create hyperparamter tune grid
# ------------------------------------------------------------------------------
svmTuneGrid <- data.frame(sigma = as.vector(sigDist)[1], C = 2^(-2:7))
svmTuneGrid


# --> 
# sigma is constant value, only Cost changes


# ------------------------------------------------------------------------------
# Training the SVM model: repeated CV (5)
#
# Optional: parallel processing can be used via the 'do' packages, such as doMC, doMPI etc.
# We used doMC (not on Windows) to speed up the computations.
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(10)

set.seed(1056)


# we use preProc (center and scale) even for 2 values variables
svmFit <- train(Class ~ ., data = data_train,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneGrid = svmTuneGrid,
                trControl = trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE))


svmFit



# ----------
# Using option tuneLength = 10, the cost values 2^-2 to 2^7 are evaluated
svmFit2 <- train(Class ~ ., data = data_train,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE))


svmFit2
svmFit2$results



# ----------
# A line plot of the average performance.
# The 'scales' argument is actually an argument to xyplot that converts the x-axis to log-2 units.
plot(svmFit, scales = list(x = list(log = 2)))

plot(svmFit2, scales = list(x = list(log = 2)))


# --> svmFit2 (not usiing sigest() parameter) is better ....


# ------------------------------------------------------------------------------
# Test set predictions
# ------------------------------------------------------------------------------
predictedClasses <- predict(svmFit, data_test)

predictedProbs <- predict(svmFit, newdata = data_test, type = "prob")


head(predictedClasses)
head(predictedProbs)



# ------------------------------------------------------------------------------
# Training the SVM model by different sampling method
# ------------------------------------------------------------------------------
# 10-fold CV
set.seed(1056)
svmFit10CV <- train(Class ~ ., data = data_train,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
#                    tuneGrid = svmTuneGrid,
                    tuneLength = 10,
                    trControl = trainControl(method = "cv", number = 10))
svmFit10CV



# ----------
# LOOCV
set.seed(1056)
svmFitLOO <- train(Class ~ ., data = data_train,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
#                   tuneGrid = svmTuneGrid,
                   tuneLength = 10,
                   trControl = trainControl(method = "LOOCV"))
svmFitLOO



# ----------
set.seed(1056)
svmFitLGO <- train(Class ~ ., data = data_train,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
#                   tuneGrid = svmTuneGrid,
                   tuneLength = 10,
                   trControl = trainControl(method = "LGOCV", number = 50, p = .8))
svmFitLGO 



# ----------
set.seed(1056)
svmFitBoot <- train(Class ~ .,
                    data = data_train,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
#                    tuneGrid = svmTuneGrid,
                    tuneLength = 10,
                    trControl = trainControl(method = "boot", number = 50))
svmFitBoot



# ----------
set.seed(1056)
svmFitBoot632 <- train(Class ~ ., data = data_train,
                       method = "svmRadial",
                       preProc = c("center", "scale"),
#                       tuneGrid = svmTuneGrid,
                       tuneLength = 10,
                       trControl = trainControl(method = "boot632", number = 50))

svmFitBoot632



# ----------
plot(svmFit10CV, scales = list(x = list(log = 2)))
plot(svmFitLOO, scales = list(x = list(log = 2)))
plot(svmFitLGO, scales = list(x = list(log = 2)))
plot(svmFitBoot, scales = list(x = list(log = 2)))
plot(svmFitBoot632, scales = list(x = list(log = 2)))



# ------------------------------------------------------------------------------
# Investigate simpler model:  logistic regression model
# ------------------------------------------------------------------------------
# The same resampling specification is used and since the random number seed is set ot modeling,
# the resamples are exactly the same as those in the SVM model.
set.seed(1056)

glmProfile <- train(Class ~ ., data = data_train,
                    method = "glm",
                    trControl = trainControl(method = "repeatedcv", repeats = 5))
glmProfile




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
# Since the random number seed was initialized prior to running the SVM and logistic models, paired accuracy measurements exist for each data set.
resamp <- resamples(list(SVM = svmFit2, Logistic = glmProfile))

summary(resamp)


modelDifferences <- diff(resamp)

summary(modelDifferences)


# -->
# p-values for the model comparisons are large (0.5682 for accuracy and 0.5334 for Kappa), 
# which indicates that the models fail to show any difference in performance.



# ----------
# The actual paired t-test:
# confidence interval for the difference of accuracy is -1.2% - 2.0%, indicating that there is no evidence to support
# the idea that the accuracy for either model is significantly better.
modelDifferences$statistics$Accuracy



# ----------
# Compare the accuracy distributions by both models
bwplot(resamp, metric="Accuracy")



# ----------
# Compare the accuracy for both models in scatter plot
xyplot(resamp)
parallelplot(resamp, metric="Accuracy")

# parallelplot(resamp, metric="Kappa")
