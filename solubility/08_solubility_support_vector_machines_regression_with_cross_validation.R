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
# Nonlinear Regression Models:  Support Vector Machines
#  - SVMs for regression use a function similar to the Huver function. Given a threshold set by the user (denoted as e: epsilon),
#    the data points with residuals within the threshold do not contribute to the regression fit while data points with an absolute difference
#    greater than the threshold contribute a linear-scale amount.
#  - Since the squared residuals are not used, large outliers have a limited effect on the regression equation.
#    Samples that the model fits well (i.e., the residuals are small) have no effect on the regression equation.
#    In fact, if the threshold is set to a relatively large value, then the outliers are the only points that define the regression line !
#  - Cost penalty penalizes large residuals (it is attached to residuals and not the parameters, like ridge regression or weight decay in neural networks)
#    The use of cost value effectively rgularizes the model to help alleviate over-parametrization problem.
#    When the cost is small, the model will become less likely to over-fit (but more likely to underfit)
#  - In experiences, the cost parameter provides more flexibility for tuning the model than the size of the funnel (epsilon), suggesting fixing a value
#    for epsilon and tuning over the other kernel parameters.
#
#  FOR PREDICTION
#  - Only a subset of training set data points are needed for prediction. Since the regression line is determined using these samples, they are called the 
#    "support vectors" as they support the regression line.
#  - When predictors enter the model linearly, the kernel function reduces to a simple sum of cross products.
#    Since the predictors enter into the model as the sum of cross products, differences in the predictor scales can affect the model.
#    Therefore, we recommend centering and scaling the predictors prior to building an SVM model.
# ------------------------------------------------------------------------------
set.seed(100)


# Grid search of 14 cost values between 2^-2, 2^-1, 2, 2^2, ..., 2^11
# Radial basis function has a parameter (sigma) that controls the scale.
# In the radial basis function, sigma is estimated using combinations of the training set points to calculate the distribution of (x - x')^2,
# then use the 10th and 90th percentiles as a range for sigma.
svmRTune <- train(x = solTrainXtrans, y = solTrainY,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneLength = 14,
                  trControl = ctrl)

svmRTune



# ----------
# the model used 638 traiing set data points as support vectors out pf 951 data points.
# kernel parameter sigma is estimated analytically to be 0.0028
svmRTune$finalModel



# ----------
plot(svmRTune, scales = list(x = list(log = 2)))                 



# ------------------------------------------------------------------------------
# polynomial SVM model
#  - We tune over the cost, the polynomial degree, and a scale factor.
#  - In general, quadratic models have smaller error rates than the linear models. Also, models associated with larger-scale factors have better performance.
# ------------------------------------------------------------------------------
svmGrid <- expand.grid(degree = 1:2, scale = c(0.01, 0.005, 0.001), C = 2^(-2:5))

set.seed(100)

svmPTune <- train(x = solTrainXtrans, y = solTrainY,
                  method = "svmPoly",
                  preProc = c("center", "scale"),
                  tuneGrid = svmGrid,
                  trControl = ctrl)


svmPTune



# ----------
# The final model was fit using a quadratic model with a scale factor of 0.01 and a cost value of 2
svmPTune$finalModel
plot(svmPTune, scales = list(x = list(log = 2), between = list(x = .5, y = 1)))                 



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testResults_rsvm <- data.frame(obs = solTestY, RSVM = predict(svmRTune, solTestXtrans))
testResults_rsvm <- testResults_rsvm %>% mutate(resid = obs - RSVM)

testResults_psvm <- data.frame(obs = solTestY, PSVM = predict(svmPTune, solTestXtrans))
testResults_psvm <- testResults_psvm %>% mutate(resid = obs - PSVM)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults_rsvm$obs, testResults_rsvm$RSVM, testResults_psvm$PSVM))

graphics.off()
par(mfrow = c(2,2))
with(testResults_rsvm, plot(obs, RSVM, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_rsvm, plot(RSVM, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testResults_psvm, plot(obs, PSVM, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_psvm, plot(PSVM, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



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
# Since the random number seed was initialized prior to running the ridge and lasso models, paired accuracy measurements exist for each data set.

resamp <- resamples(list(RSVM = svmRTune, PSVM = svmPTune))

summary(resamp)


# --> 
# almost same performance


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
# confidence interval for the difference of RMSE is -0.036 - 0.0116, indicating that there is no evidence to support
# the idea that the RMSE for PSVM is better than that for RSVM
modelDifferences$statistics$RMSE



# ----------
# resampled distribution of metric difference
trellis.par.set(theme1)
bwplot(modelDifferences, layout = c(1,3), auto.key = list(columns = 1))

trellis.par.set(caretTheme())
densityplot(modelDifferences, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 1))



