setwd("//media//kswada//MyFiles//R//sine_curve")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "partykit")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: sine curve
# ------------------------------------------------------------------------------
set.seed(100)

x <- runif(100, min = 2, max = 10)

y <- sin(x) + rnorm(length(x)) * 0.25

( sinData <- data.frame(x = x, y = y) )


graphics.off();  par(mfrow=c(1,1))

plot(x, y)



# ------------------------------------------------------------------------------
# Create folds explicitly
# ------------------------------------------------------------------------------
set.seed(100)


# create folds explicitly, default is 10 folds
indx <- createFolds(y, returnTrain = TRUE)
indx



# ------------------------------------------------------------------------------
# Set train control
# ------------------------------------------------------------------------------
ctrl <- trainControl(method = "cv", index = indx)



# ------------------------------------------------------------------------------
# Tune CART model by caret::train
# ------------------------------------------------------------------------------
set.seed(100)

cartTune <- train(x = data.frame(x), y = y, method = "rpart", tuneLength = 25, trControl = ctrl)



# ----------
# Plot the tuning results
plot(cartTune, scales = list(x = list(log = 10)))



# ----------
# In this case, the tuning process choose a largeest tree with a cp value = 0 and 11 terminal nodes.
# The estimated RMSE from this model is 0.34 (this is SMALLEST, not based on one standard error rule)
cartTune
cartTune$finalModel



# ----------
# Use the partykit package to make some nice plots. First, convert the rpart objects to party objects.
plot(as.party(cartTune$finalModel))



# ------------------------------------------------------------------------------
# Tune MARS model by caret::train
# ------------------------------------------------------------------------------
# tuning parameter:  the degree of the features that are added to the model and the number of retained terms
# nprune: maximum number of terms (including intercept) in the pruned model. Default is NULL, meaning all terms created by the forward pass
# (but typically not all terms will remain after pruning),
set.seed(100)

marsTune <- train(x = data.frame(x), y = y,
                  method = "earth",
                  tuneGrid = expand.grid(degree = 1:2, nprune = 2:20),
                  trControl = ctrl)


marsTune$finalModel


plot(marsTune)



# ----------
# h() is the hinge function. The term h(MolWeight - 5.77157) is zero when the molecular weigh is leass than 5.77157.
# selected 4 of 6 terms as described.
# Termination condition: Rsq changed by less than 0.001 at 6 terms
summary(marsTune)



# ----------
# The predicted relationship between the outcome and the continuous predictors (holding all other predictors at their mean value)
# The additive nature of the model allows each predictor to be viewed in isolation.
# Note that final predicted values are the summation of each individual profile.
plotmo(marsTune)



# ------------------------------------------------------------------------------
# Tune Bagged Trees model by caret::train
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(10)

set.seed(100)

treebagTune <- train(x = data.frame(x), y = y, method = "treebag", nbagg = 50, trControl = ctrl)

treebagTune



# ------------------------------------------------------------------------------
# Tune Bagged MARS model by caret::train
# ------------------------------------------------------------------------------
set.seed(100)

marsbagTune <- train(x = data.frame(x), y = y, method = "bagEarth", trControl = ctrl)

marsbagTune

summary(marsbagTune)



# ------------------------------------------------------------------------------
# diagnose models
# ------------------------------------------------------------------------------
testRes_cart <- data.frame(obs = y, cart = predict(cartTune, data.frame(x)))
testRes_cart <- testRes_cart %>% mutate(resid = obs - cart)

testRes_treebag <- data.frame(obs = y, treebag = predict(treebagTune, data.frame(x)))
testRes_treebag <- testRes_treebag %>% mutate(resid = obs - treebag)

testRes_mars <- data.frame(obs = y, y = predict(marsTune, data.frame(x)))
testRes_mars <- testRes_mars %>% mutate(resid = obs - y)

testRes_marsbag <- data.frame(obs = y, marsbag = predict(marsbagTune, data.frame(x)))
testRes_marsbag <- testRes_marsbag %>% mutate(resid = obs - marsbag)


# ----------
# diagnositc plot
axisRange <- extendrange(c(testRes_cart$obs, testRes_cart$cart, testRes_treebag$treebag, testRes_mars$mars, testRes_marsbag$marsbag))

graphics.off()
par(mfrow = c(2,2))
with(testRes_cart, plot(obs, cart, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_cart, plot(cart, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_treebag, plot(obs, treebag, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_treebag, plot(treebag, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)

with(testRes_mars, plot(obs, y, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_mars, plot(y, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_marsbag, plot(obs, marsbag, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_marsbag, plot(marsbag, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



# ------------------------------------------------------------------------------
# compare the predictions
# ------------------------------------------------------------------------------
# Greate a grid of x values to use for prediction
( dataGrid <- data.frame(x = seq(2, 10, length = 100)) )


# ----------
graphics.off();  par(mfrow=c(1,1))
plot(x, y)

points(x = dataGrid$x, y = predict(cartTune, dataGrid), type = "l", col = "red")
points(x = dataGrid$x, y = predict(treebagTune, dataGrid), type = "l", col = "blue")
points(x = dataGrid$x, y = predict(marsTune, dataGrid), type = "l", col = "darkgray")
points(x = dataGrid$x, y = predict(marsbagTune, dataGrid), type = "l", col = "tan")



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

resamp <- resamples(list(cart = cartTune, treebag = treebagTune, mars = marsTune, marsbag = marsbagTune))

summary(resamp)


# --> 
# mean of RMSE: marsbag --> mars --> treebag --> cart



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
modelDifferences$statistics$RMSE



# ----------
# resampled distribution of metric difference
trellis.par.set(theme1)
bwplot(modelDifferences, layout = c(1,3), auto.key = list(columns = 1))

trellis.par.set(caretTheme())
densityplot(modelDifferences, pch = "|", layout = c(2,2),  scales = "free", auto.key = list(columns = 1))
