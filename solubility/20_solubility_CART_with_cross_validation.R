setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "MASS", "rpart", "partykit")
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
# Regression Trees and Rule-Based Models:  Basic Regression Trees
#   - One of the oldest and most utilized techniques to construct regression trees is CART (classification and regression tree, by Breiman et al.(1984))
#   - For regression, the model begins with the entire data set, S, and searches every distinct value of every predictor to find the predictor and split
#     value that partitions the data into two groups (S1 and S2) such that the overall sums of squares error are minimized:
#     SSE = sum(y - y1-hat)^2 + sum(y - y2-hat)^2, where y1-hat and y2-hat are the averages of the training set outcomes within groups S1 and S2, respectively.
#   - Because of the recursive splitting nature of regression trees, this method is also known as "recursive partitioning".
#   - Once the full tree has been grown, the tree may be very large and is likely to over-fit the training set. The tree is then pruned back to a potentially
#     smaller depth. The processed used by Breiman et al. (1984) is "cost-complexity tuning".
#     SSE (cp) = SSE + cp * (# Terminal Nodes),  cp is called the "complexity parameter.  SSE(cp) is penalized by # of terminal nodes.
#   - To understand variation in SSEs for each chosen cp value, Breiman suggest using a cross-validation approach and also propose using the one-standard-error rule
#     on the optimization criteria for identifying the simplest tree: find the smallest tree that is within one standard error of the tree with smallest absolute error.
#     Another approach selects the tree size associated with the numerically smallest error (Hastie et al. 2008).
#
#   - Alternatively, the model can be tuned by choosing the value fo the complexity parameter associated with the SMALLEST possible RMSE value.
#     --> this is caret::train approach
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Fit CART model by rpart
# ------------------------------------------------------------------------------
# Fit two CART models to show the initial splitting process.
# rpart only uses formulas, so we put the predictors and outcome into a common data frame first.
trainData <- solTrainXtrans
trainData$y <- solTrainY

rpStump <- rpart(y ~ ., data = trainData, control = rpart.control(maxdepth = 1))
rpSmall <- rpart(y ~ ., data = trainData, control = rpart.control(maxdepth = 3))



# ----------
plot(as.party(rpSmall))



# ------------------------------------------------------------------------------
# Tune CART model by caret::train
# ------------------------------------------------------------------------------
set.seed(100)

cartTune <- train(x = solTrainXtrans, y = solTrainY, method = "rpart", tuneLength = 25, trControl = ctrl)



# ----------
# Plot the tuning results
plot(cartTune, scales = list(x = list(log = 10)))



# ----------
# In this case, the tuning process choose a larger tree with a cp value = 0.003 and 25 terminal nodes.
# The estimated RMSE from this model is 0.97 (this is SMALLEST, not based on one standard error rule)
cartTune
cartTune$finalModel



# ----------
# Use the partykit package to make some nice plots. First, convert the rpart objects to party objects.
plot(as.party(cartTune$finalModel))



# ----------
# Get the variable importance.
# 'competes' is an argument that controls whether splits not used in the tree should be included in the importance calculations.
# If two predictors are exteremely correlated, the choice of which to use in a split is somewhat random.
# Two surface area predictors have an extremely high correlation (0.96) and each is used in the tree.
# It is possible that the small difference between these predictors is strongly driving the choice between the two, but
# it is more liely to be due to small, random differences in the variables.
# Because of this, more predictors may be selected than actually needed.
# Including both surface area predictors in the data causes thier importance to have only moderate values.

cartImp <- varImp(cartTune, scale = FALSE, competes = FALSE)  # --> this to consider high correlated variables for importance calculation
# cartImp2 <- varImp(cartTune, scale = FALSE, competes = TRUE)

cartImp
# cartImp2

plot(cartImp, top = 20)
# plot(cartImp2, top = 20)



# ----------
# IT IS IMPORTANT:
# CART trees suffer from selection bias: predictors with a higher number of distinct values are favored over more granular predictors.
# Note that the model tends to rely more on continuou (less granular) prediuctors than the binary fingerprints.




# ------------------------------------------------------------------------------
# Apply one standard error rule
# ------------------------------------------------------------------------------
# If we apply one-standard-error rule proposed by Breiman, smallest 0.9660816 + RMSESD 0.05330954 = 1.019413 --> cp = 0.0047375
cartTune$results



# ----------
# model by this cp  --> 17 terminal nodes, more simple trees
rp <- rpart(y ~ ., data = trainData, control = rpart.control(cp = 0.0047375))

plot(as.party(rp))




# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testRes_cart <- data.frame(obs = solTestY, cart = predict(cartTune, solTestXtrans))
testRes_cart <- testRes_cart %>% mutate(resid = obs - cart)

testRes_cart2 <- data.frame(obs = solTestY, cart2 = predict(rp, solTestXtrans))
testRes_cart2 <- testRes_cart2 %>% mutate(resid = obs - cart2)



# ----------
# diagnositc plot
# cart2 (one-standard-error-rule produces only 17 possible predicted values)
axisRange <- extendrange(c(testRes_cart$obs, testRes_cart$cart, testRes_cart2$cart2))

graphics.off()
par(mfrow = c(2,2))
with(testRes_cart, plot(obs, cart, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_cart, plot(cart, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_cart2, plot(obs, cart2, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_cart2, plot(cart2, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



