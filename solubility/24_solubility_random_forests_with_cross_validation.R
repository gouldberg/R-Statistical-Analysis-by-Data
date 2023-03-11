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


# rpart only uses formulas, so we put the predictors and outcome into a common data frame first.
trainData <- solTrainXtrans
trainData$y <- solTrainY



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
# Regression Trees and Rule-Based Models:  Random Forests
#   - The trees in bagging are not completely independent of each other since all of the original predictors are considered at every split of every tree.
#     Breiman (2001) constructed a unified algorithm called "random forests" to reduce correlation among trees ("de-correlating trees")
#     This algorithm randomly selects predictors at "each split", tree correlation will necessarily be lessened.
#     -------------------------
#     Random Forests algorithm:
#     Select the number of models to build, m
#     for i = 1 to m do
#         Generage a bootstrap sample of the original data
#         Train a tree model on this sample
#             for "each split" Do
#                 Randomly select k (< P) of the original predictors
#                 Select the best predictor among the k predictors and partition the data
#             end
#         Use typical tree model stopping criteria to determine when a tree is complete (but do not prune)
#     end
#     -------------------------
#
#   - Breiman (2001) recommends setting mtry to be one-third of the number of predictors
#     For the purpose of tuning the mtry parameter, since random forests is computationally intensive, we suggest starting with 5 values of k
#     that are somewhat evenly spaced across the range from 2 to P
#
#   - As starting point, we suggest using at least 1,000 trees. If the cross-validation performance profiles are still improving at 1,000 trees,
#     then incorporate more trees until performance levels off.
#
#   - Because each learner is selected independently of all previous learners, random forests is robust to a noisy response.
#     But aat the same time, the independence of learners can underfit data when the response is not noisy.
#
#   - Like bagging, CART or conditional inference trees can be used as the base learner in random forests.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Tune Random Forests (CART) model by cross-validation
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(10)


# The mtry parameter will be evaluated at 10 values from 10 to 328
( mtryGrid <- data.frame(mtry = floor(seq(10, ncol(solTrainXtrans), length = 10))) )



# ----------
# Tune the model using cross-validation:  IT TAKES TIME !!!: 20 minutes ..
set.seed(100)

rfTune <- train(x = solTrainXtrans, y = solTrainY, method = "rf", tuneGrid = mtryGrid,
                ntree = 1000, importance = TRUE, trControl = ctrl)

rfTune



# ----------
# the CART-based random forest model is numerically optimal at mtry = 155
plot(rfTune)



# ----------
# variable importance:  varImp is wrapper for variable importance functions ("importance")
# package randomForest's variable importance measures 
#   - mean decrease in accuracy:  computed from permuting OOB data. For each tree, the prediction error on the out-of-bag portaion of the data
#     is recorded (error rate for classification, MSE for regression). Then the same is done after permuting each predictor variable.
#     The difference between the two are then averaged over all trees, and normalized by the standard deviation of the differences.
#     If the standard deviation of the differences is equal to zero for a variable, the division is not done.
#   - mean decrease in node purity:  the total decrease in node impurities from splitting on the variable, averaged over all trees.
#     For classification, the node impuritu is measured by the Gini index. For regression, it is measured by residual sum of squares.
# But this approach suffers from the same limitations related to bias. Uninformative predictors with high correlations to informative predictors
# had abnormally large importance values. In some cases, their importance was greater than or equal to weakly important variables.
# Also the mtry tuning parameter has a serious effect on the importance values.
# Strobl et al. (2007) developed an alternative approach for calculating importance in random forest models that takes between-predictor correlations
# into account. Their methodology reduces the effect of between-predictor redundancy.
rfImp <- varImp(rfTune, scale = FALSE)

plot(rfImp, top = 40)



# ------------------------------------------------------------------------------
# Tune Random Forests (CART) model using the OOB estimate
# ------------------------------------------------------------------------------
ctrlOOB <- trainControl(method = "oob")

set.seed(100)

rfTuneOOB <- train(x = solTrainXtrans, y = solTrainY, method = "rf", tuneGrid = mtryGrid,
                   ntree = 1000, importance = TRUE, trControl = ctrlOOB)


rfTuneOOB


# ----------
# the CART-based random forest model using the OOB estimate is numerically optimal at mtry = 82
plot(rfTuneOOB)



# ----------
# variable importance:  varImp is wrapper for variable importance functions ("importance")
rfOOBImp <- varImp(rfTuneOOB, scale = FALSE)

plot(rfOOBImp, top = 40)



# ------------------------------------------------------------------------------
# Tune the conditional inference forests model by cross-validation
# ------------------------------------------------------------------------------
library(party)

# "controls" (not the plural) allows the user to pick the type of splitting algorithm to use (biased or unbiased)
# Note that these functions cannot be used with missing data.

set.seed(100)


# IT TAKES TIME !!!:  15 minutes ...
condrfTune <- train(x = solTrainXtrans, y = solTrainY, method = "cforest", tuneGrid = mtryGrid,
                    controls = party::cforest_unbiased(ntree = 1000), trControl = ctrl)


condrfTune



# ----------
# the ctree-based random forest model is numerically optimal at mtry = 131
plot(condrfTune)



# ----------
# variable importance:  varImp is wrapper for variable importance functions ("importance")
condrfImp <- varImp(condrfTune, scale = FALSE)

plot(condrfImp, top = 40)



# ------------------------------------------------------------------------------
# Tune the conditional inference forests model using the OOB estimate
# ------------------------------------------------------------------------------
set.seed(100)

condrfTuneOOB <- train(x = solTrainXtrans, y = solTrainY, method = "cforest", tuneGrid = mtryGrid,
                       controls = party::cforest_unbiased(ntree = 1000),
                       trControl = trainControl(method = "oob"))

condrfTuneOOB



# ----------
# the ctree-based random forest model using the OOB estimate is numerically optimal at mtry = 106
plot(condrfTuneOOB)



# ----------
# variable importance:  varImp is wrapper for variable importance functions ("importance")
condrfOOBImp <- varImp(condrfTuneOOB, scale = FALSE)

plot(condrfOOBImp, top = 40)



# ------------------------------------------------------------------------------
# Compare cross-validated RMSE profile for the CART and conditional inference approarches to random forests
# ------------------------------------------------------------------------------
output1 <- rfTune$results[,c("mtry","RMSE")] %>% mutate(class = "rfTune")
output2 <- rfTuneOOB$results[,c("mtry","RMSE")] %>% mutate(class = "rfTuneOOB")
output3 <- condrfTune$results[,c("mtry","RMSE")] %>% mutate(class = "condrfTune")
output4 <- condrfTuneOOB$results[,c("mtry","RMSE")] %>% mutate(class = "condrfTuneOOB")

output <- rbind(output1, output2, output3, output4)



# ----------
# The CART-based random forest model is numerically optimal at mtry = 82 to 155 regardless of the method of estimating the RMSE.
# Breiman (2001) recommends setting mtry to be one-third of the number of predictors. In this case, 228 * 1/3 = 76

# Experience shows that the random forest tuning parameter does not have a drastic effect on performance.
# In this data, the only real difference in the RMSE comes when the smallest value is used (10 in this case)

# Also note that random forest models built with CART trees had extremely similar RMSE results with the out-of-bag error estimate and cross-validation.
# (when compared across tuning parameters)
# Using the out-of-bag error rate would drastically decrease the computational time to tune random forest models.
# For forests created using conditional inference trees, the out-of-bag error was much more optimistic than the cross-validated RMSE.
xyplot(RMSE ~ mtry, type = "b", groups = class, data = output, auto.key = list(columns = 4), col = c("black", "darkgray", "blue", "red"))



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testRes_rf <- data.frame(obs = solTestY, rf = predict(rfTune, solTestXtrans))
testRes_rf <- testRes_rf %>% mutate(resid = obs - rf)

testRes_condrf <- data.frame(obs = solTestY, condrf = predict(condrfTune, solTestXtrans))
testRes_condrf <- testRes_condrf %>% mutate(resid = obs - condrf)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testRes_rf$obs, testRes_rf$rf, testRes_condrf$condrf))

graphics.off()
par(mfrow = c(2,2))
with(testRes_rf, plot(obs, rf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_rf, plot(rf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_condrf, plot(obs, condrf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_condrf, plot(condrf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



# ------------------------------------------------------------------------------
# train boosted trees by randomForest
# ------------------------------------------------------------------------------
library(randomForest)

rfModel <- randomForest(solTrainXtrans, solTrainY, importance = TRUE, ntrees = 1000, mtry = 82)

# or
rfModel <- randomForest(y ~., data = trainData, importance = TRUE, ntrees = 1000, mtry = 82)



