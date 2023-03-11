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
# Nonlinear Regression Models:  Multivariate Adaptive Regression Splines
#   - Whereas PLS and neural networks are based on linear combinations of the predictors, MARS creates two contrasted versions of a predictor to enter the model.
#   - The surrogate features in MARS are usually a function of only one or two predictors ata time. The nature of the MARS features breaks the predictor
#     into 2 groups and models linear relationships between the predictor and the outcome in each group.
#     Specifically, given a cut point for a predictor, 2 new features are "hinge" or "hockey stick" functions of the original.
#     In effect, this scheme creates a pecewise linear model where each new feature models an isolated portion of the origianl data.
#   - Each data point for each predictor is evaluated as a candidate cut point by creating a linear regression model with the candidate features,
#     and the corresponding model error is calculated. The predictor/cut point combination that achieves the smallest error is then used for the model.
#     After the initial model is created with the 1st 2 features, the model conducts another exhaustive search to find the next set of features that,
#     given the initial set, yield the best model fit. This process continues until a stopping point si reached.
#   - To determine the contribution of each feature to the model, the GCV (generalized cross-validation) statistic is used.
#     This value is a computational shorcut for linear regression models that produces an error value that approximates leave-one-out cross-validation.
#   - MARS can build models where the features involve multiple predictors at once. (2 predictors at once is "second-degree" MARS)
#     For MARS models that can include 2 or more terms at a time, observed are occasional instabilities in the model predictions where a few sample predictions
#     are wildly inaccurate. (This problem is alleviated by additive MARS models)
#
# Advantages of MARS model
#   - automatic feature selection
#   - interpretability
#   - requires very little pre-processing of the data 
#       - a zero variance predictor will never be chosen for a split
#       - correlated predictors do not drastically affect model performance (but they can complicate interpretation, since random choice of predictors)
# ------------------------------------------------------------------------------
# tuning parameter:  the degree of the features that are added to the model and the number of retained terms
set.seed(100)


marsTune <- train(x = solTrainXtrans, y = solTrainY,
                  method = "earth",
                  tuneGrid = expand.grid(degree = 1:2, nprune = 2:38),
                  trControl = ctrl)


marsTune$finalModel
plot(marsTune)



# ----------
# h() is the hinge function. The term h(MolWeight - 5.77157) is zero when the molecular weigh is leass than 5.77157.
summary(marsTune)



# ----------
# variable importance by tracking the reduction in the root mean squared error (as measured using the GCV statistic)
# that occurs when adding a particular feature to the model
# For this data, MolWeight, NumNonHAtoms, and SurfaceArea1 appear to be have the greatest influence on the MARS model.
marsImp <- varImp(marsTune, scale = FALSE)

plot(marsImp, top = 25)



# ----------
# The predicted relationship between the outcome and the continuous predictors (holding all other predictors at their mean value)
# The additive nature of the model allows each predictor to be viewed in isolation.
# Note that final predicted values are the summation of each individual profile.
plotmo(marsTune)



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testResults_mars <- data.frame(obs = solTestY, y = predict(marsTune, solTestXtrans))
testResults_mars <- testResults_mars %>% mutate(resid = obs - y)



# ----------
# diagnositc plot
axisRange <- extendrange(c(testResults_mars$obs, testResults_mars$y))

graphics.off()
par(mfrow = c(1,2))
with(testResults_mars, plot(obs, y, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testResults_mars, plot(y, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)




testResults$MARS <- predict(marsTune, solTestXtrans)


