# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret", "pROC", "partykit", "rpart")
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
# Create variable for factor predictos
# ------------------------------------------------------------------------------
# In the classification tree, there is a different set of predictors that use factor encodings of some of the predictors

factorPredictors <- names(training)[names(training) != "Class"]
factorPredictors <- factorPredictors[!grepl("Sponsor[0-9]", factorPredictors)]
factorPredictors <- factorPredictors[!grepl("SponsorUnk", factorPredictors)]
factorPredictors <- factorPredictors[!grepl("ContractValueBand[A-Z]", factorPredictors)]
factorPredictors <- factorPredictors[!grepl("GrantCat", factorPredictors)]
factorPredictors <- factorPredictors[!(factorPredictors %in% levels(training$Month))]
factorPredictors <- factorPredictors[!(factorPredictors %in% levels(training$Weekday))]

# factorForm <- paste("Class ~ ", paste(factorPredictors, collapse = "+"))
# factorForm <- as.formula(factorForm)



# ------------------------------------------------------------------------------
# Set train control
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
# Classification Trees and Rule-Based Models:  CART
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(cores = 20)



# ----------
# CART model with categorical predictors treated as cohesive sets (grouped categories)
#   - Each categorical predictor is entered into the model as a single entity so that the model decides how to group or split the values
set.seed(476)

rpartFactorFit <- train(x = training[,factorPredictors], y = training$Class, method = "rpart", tuneLength = 30,
                        metric = "ROC", trControl = ctrl)

rpartFactorFit

update(plot(rpartFactorFit), ylab = "ROC AUC (2008 Hold-Out Data)")

summary(rpartFactorFit)

plot(as.party(rpartFactorFit$finalModel))



# ----------
# CART model with independent categories:
#   - Categorical predictors decomposed into binary dummy variables to be considered independently forcing binary splits
set.seed(476)

rpartFit <- train(x = training[,fullSet], y = training$Class, method = "rpart", tuneLength = 30,
                  metric = "ROC", trControl = ctrl)

rpartFit

update(plot(rpartFit), ylab = "ROC AUC (2008 Hold-Out Data)")

summary(rpartFit$finalModel)

plot(as.party(rpartFit$finalModel))



# -->
# Because the approach to use independent category predictors creates many more predictors,
# we would expect that the pruned would have more terminal nodes.
# Counter to intuition, the final pruned tree has only 16 nodes



# ----------
# confusion matrix and other statistics
confusionMatrix(rpartFactorFit, norm = "none")
confusionMatrix(rpartFit, norm = "none")



# ----------
# ROC Curve
rpartFactorFit$pred <- merge(rpartFactorFit$pred,  rpartFactorFit$bestTune)
rpartFit$pred <- merge(rpartFit$pred,  rpartFit$bestTune)


rpartFactor_roc <- roc(response = rpartFactorFit$pred$obs, predictor = rpartFactorFit$pred$successful, levels = rev(levels(rpartFactorFit$pred$obs)))
rpart_roc <- roc(response = rpartFit$pred$obs, predictor = rpartFit$pred$successful, levels = rev(levels(rpartFit$pred$obs)))


plot(rpart_roc, type = "s", print.thres = c(.5), print.thres.pch = 3, print.thres.pattern = "", print.thres.cex = 1.2, col = "red", print.thres.col = "red", legacy.axes = TRUE)
plot(rpartFactor_roc, type = "s", print.thres = c(.5), print.thres.pch = 16, print.thres.pattern = "", print.thres.cex = 1.2, add = TRUE, legacy.axes = TRUE)
legend(.75, .2, c("Grouped Categories", "Independent Categories"), lwd = c(1, 1), col = c("black", "red"), pch = c(16, 3))


auc(rpartFactor_roc);  auc(rpart_roc)
ci(rpartFactor_roc);  ci(rpart_roc)


# -->
# optimal AUC for CART model with independent categories was 0.912



# ----------
# variable importance
plot(varImp(rpartFit, scale = FALSE), top=20, scales = list(y = list(cex = .95)))



# ------------------------------------------------------------------------------
# model by rpart package
# ------------------------------------------------------------------------------
cartModel <- rpart(Class ~ ., data = training[pre2008, c("Class", fullSet)])

cartModel

summary(cartModel)

plot(as.party(cartModel))



# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

rpart_ho_pred_prob <- predict(rpartFit, newdata = training[-pre2008, fullSet], type ="prob")
rpart_ho_pred <- predict(rpartFit, newdata = training[-pre2008, fullSet], type ="raw")

head(rpart_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
rpart_ho_roc <- pROC::roc(response = training[-pre2008, "Class"], predictor = rpart_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(rpart_ho_roc, legacy.axes = TRUE, col = "black")

pROC::auc(rpart_ho_roc)



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_rpart <- data.frame(obs = training[-pre2008, "Class"], rpart_ho_pred_prob = rpart_ho_pred_prob$successful, rpart_ho_pred = rpart_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ rpart_ho_pred_prob | obs, data = testRes_rpart,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

