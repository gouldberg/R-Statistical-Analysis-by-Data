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
# Regression Trees and Rule-Based Models:  Conditional Inference Trees
#   - While trees are highly interpretable and easy to compute, they do have some noteworthy disadvantages.
#     (1) Single regression trees are more likely to have sub-optimal predictive performance compared to other modeling approaches.
#         By construction, tree models partition the data into rectangular regions of the predictor space. If the relationship between predictors
#         and the outcome is not adequately described by these rectangles, then the predictive performance of a tree will not be optimal.
#     (2) The number of possible predicted outcomes from a tree is finite and is determined by the number of terminal nodes.
#     (3) Individual tree tends to be unstable. If the data are slightly altered, a completely different set of splits might be found (i.e., the model variance is high)
#     (4) These trees suffer from selection bias: predictors with a higher number of distinct values are favored over more granular predictors.
#         The danger occurs when a data set consists of a mix of informative and noise variables, and the noise variables have may more splits than the informative variables.
#         There is a high probability that the noise variables will be chosen to split the top nodes of the tree. Pruning will produce either a tree with misleading structure
#         or no tree at all.
#         Also, as the number of missing values increases, the selection of predictors becomes more biased.
#
#   - Tackling the (4) (selection bias), Loh (2002) proposed the generalized, unbiased, interaction detection and estimation (GUIDE) algorithm
#     which solves the problem by decoupling the process of selecting the split variable and the split value. This algorithm ranks the predictors
#     using statistical hypothesis testing and then finds the appropriate split value associated with the most important factor.
#
#   - Another approach is conditional inference trees of Hothorn et al. (2006). 
#     They described a unified framework for unbiased tree-based models for regression, classification, and other scenarios.
#     In this model, statistical hypothesis tests are used to do an exhaustive search across the predictors and their possible split points.
#     For a candidate split, a statistical test is used to evauate the difference between the means of the 2 groups created by the split and a p-value
#     can be computed for the test.
#     Utilizing the test statistic p-value has several advantages.
#        - (1) p-values are on the same scale, enabling compare predictors on disparate scales.
#        - (2) multiple comparison corrections:  reducing the number of false-positive test results.
#              Predictors are increasingly penalized by multiple comparison procedures as the number of splits (and associated p-values) increases.
#              For this reason, the bias is rduced for highly granular data. A threshold for statistical significance is used to determine whether
#              additional splits should be created (Hothorn et al. (2006) use one minus the p-value)
#     By default, this algorithm does not use pruning; as the data sets are further split, the decrease in the number of samples reduces the power of the hypothesis tests.
#     However, statistical hypothesis tests are not directly related to predictive performance, and , because of this, it is still advisable to choose
#     the complexity of the tree on the basis of performance. (via resampling or some other means)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Tune cTree model by caret::train
# ------------------------------------------------------------------------------
# set the threshold for statistical significance for hypothesis tests (false-positive rate) to determine whether additional splits should be created.
# Hotthorn use one minus this p-value
( cGrid <- data.frame(mincriterion = sort(c(.95, seq(.75, .99, length = 15)))) )

set.seed(100)

ctreeTune <- train(x = solTrainXtrans, y = solTrainY, method = "ctree", tuneGrid = cGrid, trControl = ctrl)

ctreeTune

plot(ctreeTune)

ctreeTune$finalModel               


# ----------
# this model object does not need as.party()
plot(ctreeTune$finalModel)



# -->
# The tree size associated with the smallest error had 36 terminal nodes (using a threshold of 0.853)
# The tree is much larger than the basic regression tree (by CART)



# ----------
# Get the variable importance.
# 'competes' is an argument that controls whether splits not used in the tree should be included in the importance calculations.
# If two predictors are exteremely correlated, the choice of which to use in a split is somewhat random.
# Two surface area predictors have an extremely high correlation (0.96) and each is used in the tree.
# It is possible that the small difference between these predictors is strongly driving the choice between the two, but
# it is more liely to be due to small, random differences in the variables.
# Because of this, more predictors may be selected than actually needed.
# Including both surface area predictors in the data causes thier importance to have only moderate values.

ctreeImp <- varImp(ctreeTune, scale = FALSE, competes = FALSE)  # --> this to consider high correlated variables for importance calculation
# ctreeImp2 <- varImp(ctreeTune, scale = FALSE, competes = TRUE)

ctreeImp
# ctreeImp2

plot(ctreeImp, top = 20)
# plot(ctreeImp2, top = 20)


# -->
# binary variable (fingerprints) more ranks in top 10 than the model by CART



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testRes_ctree <- data.frame(obs = solTestY, ctree = predict(ctreeTune, solTestXtrans))
testRes_ctree <- testRes_ctree %>% mutate(resid = obs - ctree)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testRes_ctree$obs, testRes_ctree$ctree))

graphics.off()
par(mfrow = c(1,2))
with(testRes_ctree, plot(obs, ctree, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_ctree, plot(ctree, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



# -->
# Better than fitting by CART model
# but does not do a good job predicting samples whose true outcomes are extremely high or low
# due to limitation of simple regression trees in that each terminal mode uses the average of the training set outcomes in that node for prediction


