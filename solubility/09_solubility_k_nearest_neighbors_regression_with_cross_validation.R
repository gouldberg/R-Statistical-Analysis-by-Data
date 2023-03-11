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
# Nonlinear Regression Models:  k-Nearest-Neighbors regression
#  - Because the KNN model fundamentally depends on distance between samples, the scale of the predictors can have a dramatic influence
#    on the distances among samples. Recommended is that all predictors be centered and scaled prior to performing KNN.
#  - The KNN method can have poor predictive performance when local predictor structure is not relevant to the response. Removing irrelevant,
#    noise-laden predictors is a key pre-processing step for KNN.
#  - Enhancing KNN predictivity is to weight the neighbors' contribution to the prediction of a new sample based on their distance to the new sample.
#    Training samples that are closer to the new sample contribute more to the predicted response, while those that are farther away contribute less to the
#    predicted response.
#  - One can replace the original data with a less memory-intensive representation of the data that describes the locations of the original data.
#    A k-dimensional tree orthogonally partitions the predictor space using a tree approach but with different rules than the kinds of trees.
#    After the tree has been grown, a new sample is placed through the structure. Distances are only computed for those training observations in the tree
#    that are close to the new sample. This approach provides significant computational improvements, especially when the number of training samples
#    is much larger than the number of predictors.
# ------------------------------------------------------------------------------
set.seed(100)


# remove near zero variance predictors
knnDescr <- solTrainXtrans[, -nearZeroVar(solTrainXtrans)]


set.seed(100)

knnTune <- train(x = knnDescr, y = solTrainY,
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(k = 1:20),
                 trControl = ctrl)


knnTune



# ----------
# RMSE (cross-validation) is smallest at k = 4
plot(knnTune)



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
# when predicting new samples using this knnTune object, the new samples are automatically centered and scaled using the values
# determined by the training set

testResults_knn <- data.frame(obs = solTestY, knn = predict(knnTune, solTestXtrans[, names(knnDescr)]))
testResults_knn <- testResults_knn %>% mutate(resid = obs - knn)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults_knn$obs, testResults_knn$knn))

graphics.off()
par(mfrow = c(1,2))
with(testResults_knn, plot(obs, knn, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_knn, plot(knn, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

