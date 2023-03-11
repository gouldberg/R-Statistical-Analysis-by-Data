setwd("//media//kswada//MyFiles//R//quad_boundary")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "MASS", "randomForest")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Simulate some two class data with two predictors
# ------------------------------------------------------------------------------
set.seed(975)

training <- quadBoundaryFunc(500)
testing <- quadBoundaryFunc(1000)


str(training)
str(testing)

testing$class2 <- ifelse(testing$class == "Class1", 1, 0)
testing$ID <- 1:nrow(testing)



# ------------------------------------------------------------------------------
# Different shape (or color) by probabilities
# ------------------------------------------------------------------------------
training$prob2 <- cut(training$prob, breaks = seq(0, 1, by = 0.2))

xyplot(X2 ~ X1 | class, data = training, groups = prob2)



# ------------------------------------------------------------------------------
# Quadratic discriminant model and random forest model
# ------------------------------------------------------------------------------
# Quadratic discriminant model
qdaFit <- MASS::qda(class ~ X1 + X2, data = training)

qdaTrainPred <- predict(qdaFit, data = training)

trainRes_qda <- data.frame(obs = training$prob, qda = qdaTrainPred$posterior[,"Class1"]) %>% mutate(resid = obs - qda)



# ----------
# Random Forest
rfFit <- randomForest(class ~ X1 + X2, data = training, ntree = 2000)

rfTrainPred <- predict(rfFit, data = training, type = "prob")

trainRes_rf <- data.frame(obs = training$prob, rf = rfTrainPred[,"Class1"]) %>% mutate(resid = obs - rf)



# ----------
axisRange <- extendrange(c(trainRes_qda$obs, trainRes_qda$qda, trainRes_rf$rf))

graphics.off()
par(mfrow = c(2,2))
with(trainRes_qda, plot(obs, qda, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_qda, plot(qda, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(trainRes_rf, plot(obs, rf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(trainRes_rf, plot(rf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



# ------------------------------------------------------------------------------
# Prediction for test data
# ------------------------------------------------------------------------------
testing$qda_prob <- predict(qdaFit, testing)$posterior[,"Class1"]
testing$qda_class <- predict(qdaFit, testing)$class
testing$rf_prob <- predict(rfFit, testing, type = "prob")[,"Class1"]
testing$rf_class <- predict(rfFit, testing)

testRes_qda <- data.frame(obs = testing$prob, qda = testing$qda_prob) %>% mutate(resid = obs - qda)
testRes_rf <- data.frame(obs = testing$prob, rf = testing$rf_prob) %>% mutate(resid = obs - rf)


# ----------
axisRange <- extendrange(c(testRes_qda$obs, testRes_qda$qda, testRes_rf$rf))

graphics.off()
par(mfrow = c(2,2))
with(testRes_qda, plot(obs, qda, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_qda, plot(qda, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)
with(testRes_rf, plot(obs, rf, ylim = axisRange, xlim = axisRange, pch="*", col="red"));  abline(0, 1, col = "darkgrey", lty = 2)
with(testRes_rf, plot(rf, resid, pch="*", col = "blue"));  abline(h = 0, col = "darkgrey", lty = 2)



# ------------------------------------------------------------------------------
# Predicted class:  Confusion matrix and various statistics
# ------------------------------------------------------------------------------
confusionMatrix(data = testing$qda_class, reference = testing$class, positive = "Class1")

confusionMatrix(data = testing$rf_class, reference = testing$class, positive = "Class1")



# ------------------------------------------------------------------------------
# Receiver Operation Charasteristic Curve (ROC Curve)
# ------------------------------------------------------------------------------
library(pROC)


# This function assumes that 2nd class is the event of interest, so we reverse the labels
rocCurve_qda <- roc(response = testing$class, predictor = testing$qda_prob, levels = rev(levels(testing$class)))

rocCurve_rf <- roc(response = testing$class, predictor = testing$rf_prob, levels = rev(levels(testing$class)))



# ----------
# AUC and its confidence intervals
auc(rocCurve_qda)
auc(rocCurve_rf)

ci(rocCurve_qda)
ci(rocCurve_rf)


# ----------
# ROC curve
# By default, the x-axis goes backwards, used the option legacy.axes = TRUE to get 1 - specificity on the x-axis moving from 0 to 1
graphics.off();  par(mfrow=c(1,1));
plot(rocCurve_qda, legacy.axes = TRUE, col = "blue")
plot(rocCurve_rf, legacy.axes = TRUE, add = TRUE, col = "red")



# ------------------------------------------------------------------------------
# Lift Charts
# ------------------------------------------------------------------------------
labs <- c(rf_prob = "Random Forest", qda_prob = "Quadratic Discriminant Analysis")

liftCurve <- lift(class ~ rf_prob + qda_prob, data = testing, labels = labs)

liftCurve



# ----------
# To plot 2 lift curves, the xyplot function is used to create a lattice plot
xyplot(liftCurve, auto.key = list(columns = 2, lines = TRUE, points = FALSE))



# ------------------------------------------------------------------------------
# Calibration analysis
# ------------------------------------------------------------------------------
calData1 <- calibration(class ~ qda + rf, data = testing, cuts = 10)

calData1$data



# ----------
# Plot the observed event percentage by bin Midpoint
# Note that xyplot accepts "calibration" class data !! 

xyplot(calData1, auto.key = list(columns = 2))



# ------------------------------------------------------------------------------
# Calibrate the predicted probabilities for test data
# ------------------------------------------------------------------------------
# To calibrate the data, treat the probabilities as inputs into the model
trainProbs <- training

trainProbs$qda <- predict(qdaFit)$posterior[,"Class1"]



# ----------
# Bayesian approach for calibration (naive Bayes model)
# The option usekernel = TRUE allows a flexible function to model the probability distribution of the class probabilities
library(klaR)
nbCal <- NaiveBayes(class ~ qda, data = trainProbs, usekernel = TRUE)

BayesProbs <- predict(nbCal, newdata = testing[, "qda_prob", drop = FALSE])



# ----------
# Sigmoidal transformation
# We use relevel() here because glm() models the probability of the second factor level.
lrCal <- glm(relevel(class, "Class2") ~ qda, data = trainProbs, family = binomial)
coef(summary(lrCal))



# ----------
# Now re-predict the test set using the modified class probability estimates
testing$qda_nbcal <- predict(nbCal, testing[, "qda", drop = FALSE])$posterior[,"Class1"]
testing$qda_lrcal <- predict(lrCal, testing[, "qda", drop = FALSE], type = "response")



# ----------
calData2 <- calibration(class ~ qda + qda_nbcal + qda_lrcal, data = testing)

xyplot(calData2, auto.key = list(columns = 1))




