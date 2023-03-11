rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("plyr", "dplyr", "caret", "reshape2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ----------
library(doMC)
cores <- 20
registerDoMC(cores)



# ----------
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/training")
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/testing")
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/fullSet")
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/reducedSet")
load(file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/pre2008")



# ------------------------------------------------------------------------------
# Logistic Regression (demonstration):  Class ~ Yday  and  Class ~ Yday^2
# ------------------------------------------------------------------------------
# Note that glm models the probability of the second factor level (in this case "unsuccessful")
unique(training$Yday)

modelFit <- glm(Class ~ Yday, data = training[pre2008,], family = binomial)

summary(modelFit)



# ----------
dataGrid <- data.frame(Yday = seq(0, 365, length = 500))

dataGrid$Linear <- 1 - predict(modelFit, dataGrid, type = "response")

linear2008 <- auc(roc(response = training[-pre2008, "Class"],
                      predictor = 1 - predict(modelFit, training[-pre2008,], type = "response"),
                      levels = rev(levels(training[-pre2008, "Class"]))))


# ----------
# Class ~ Day^2
modelFit2 <- glm(Class ~ Yday + I(Yday^2), data = training[pre2008,], family = binomial)

summary(modelFit2)


# ----------
dataGrid$Quadratic <- 1 - predict(modelFit2, dataGrid, type = "response")

quad2008 <- auc(roc(response = training[-pre2008, "Class"],
                    predictor = 1 - predict(modelFit2, training[-pre2008,], type = "response"),
                    levels = rev(levels(training[-pre2008, "Class"]))))


# ----------
dataGrid <- melt(dataGrid, id.vars = "Yday")

byDay <- training[pre2008, c("Yday", "Class")]

byDay$Binned <- cut(byDay$Yday, seq(0, 360, by = 5))

observedProps <- ddply(byDay, .(Binned), function(x) c(n = nrow(x), mean = mean(x$Class == "successful")))
observedProps$midpoint <- seq(2.5, 357.5, by = 5)



# ----------
# xyplot with line + xyplot with scatter point
xyplot(value ~ Yday | variable, data = dataGrid,
       ylab = "Probability of A Successful Grant",
       ylim = extendrange(0:1),
       between = list(x = 1),
       panel = function(...)
       {
         panel.xyplot(x = observedProps$midpoint, observedProps$mean,
                      pch = 16., col = rgb(.2, .2, .2, .5))
         panel.xyplot(..., type = "l", col = "black", lwd = 2)
       })



# ------------------------------------------------------------------------------
# Add squared day of the year as variable
# ------------------------------------------------------------------------------
# add yday^2 variable
training$Yday2 <- training$Yday^2
testing$Yday2 <- testing$Yday^2

fullSet <- c(fullSet, "Yday2")
reducedSet <- c(reducedSet, "Yday2")



# save(training, file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/training")
# save(testing, file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/testing")
# save(fullSet, file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/fullSet")
# save(reducedSet, file = "/media/kswada/MyFiles/data/kaggle_grant_applications/mart_engineered/reducedSet")



# ------------------------------------------------------------------------------
# Set trainControl
# ------------------------------------------------------------------------------
# This control object will be used across multiple models so that the data splitting is consistent
# Train must know exactly which samples to use when estimating parameters.
# The "index" argument to trainControl identifies these samples. For any sampling method, a set of holdout samples can be exactly specified.
# For example, with 10-fold cross-validation, the exact samples to be excluded for each of the 10-folds are identified with this option.
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     index = list(TrainSet = pre2008),
                     savePredictions = TRUE)



# ------------------------------------------------------------------------------
# Logistic Regression trained with metric = "ROC":  For reduced set and full set
# ------------------------------------------------------------------------------
# ctrl (= trainControl) controls hold-out samples (pre2008) to be excluded
set.seed(476)
lrFit_red <- train(x = training[, reducedSet], y = training$Class, method = "glm", metric = "ROC", trControl = ctrl)


# IT TAKES TIME:  
set.seed(476)
lrFit_full <- train(x = training[, fullSet], y = training$Class, method = "glm", metric = "ROC", trControl = ctrl)


summary(lrFit_red)
summary(lrFit_full)



# ----------
# confusion matrix and other statistics
confusionMatrix(lrFit_red, norm = "none")
confusionMatrix(lrFit_full, norm = "none")



# ----------
# ROC Curve
#  --> removal of the near-zero variance predictors has a positive effect on the model fit
lrFit_red$pred <- merge(lrFit_red$pred,  lrFit_red$bestTune)
lrFit_full$pred <- merge(lrFit_full$pred,  lrFit_full$bestTune)

lr_red_roc <- roc(response = lrFit_red$pred$obs, predictor = lrFit_red$pred$successful, levels = rev(levels(lrFit_red$pred$obs)))
lr_full_roc <- roc(response = lrFit_full$pred$obs, predictor = lrFit_full$pred$successful, levels = rev(levels(lrFit_full$pred$obs)))

plot(lr_red_roc, legacy.axes = TRUE, col = "blue")
plot(lr_full_roc, legacy.axes = TRUE, add = TRUE, col = "red")

auc(lr_red_roc);  auc(lr_full_roc);
ci(lr_red_roc);  ci(lr_full_roc);



# -->
# Many of the categorical predictors have sparse and unbalanced distributions.
# Because of this, we would expect that a model using the full set of predictors would perform worse than the set that has near-zero
# variance predictors removed.

# For the reduced set of 253(256?) variables, the AUC was 0.87, the sensitivity was 81.23%, and the specificity was 80.85%



# ------------------------------------------------------------------------------
# Variable Importance
# ------------------------------------------------------------------------------
plot(varImp(lrFit_red, scale = FALSE), top = 20)



# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

lr_red_ho_pred_prob <- predict(lrFit_red, newdata = training[-pre2008, reducedSet], type ="prob")
lr_red_ho_pred <- predict(lrFit_red, newdata = training[-pre2008, reducedSet], type ="raw")

head(lr_red_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
lr_red_ho_roc <- roc(response = training[-pre2008, "Class"], predictor = lr_red_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(lr_full_roc, legacy.axes = TRUE, col = "darkgray")
plot(lr_red_roc, legacy.axes = TRUE, add = TRUE, col = "blue")
plot(lr_red_ho_roc, legacy.axes = TRUE, add = TRUE, col = "red")

auc(lr_red_roc)
auc(lr_red_ho_roc)


# --> predictions for hold-out data are better



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_lr <- data.frame(obs = training[-pre2008, "Class"], lr_red_ho_pred_prob = lr_red_ho_pred_prob$successful, lr_red_ho_pred = lr_red_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ lr_red_ho_pred_prob | obs, data = testRes_lr,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")




