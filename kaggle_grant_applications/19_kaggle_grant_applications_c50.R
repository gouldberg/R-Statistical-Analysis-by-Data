# rm(list=ls())

setwd("//media//kswada//MyFiles//R//kaggle_grant_applications")

packages <- c("dplyr", "caret", "pROC", "C50")
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
# Classification Trees and Rule-Based Models:  C5.0
#   - C5.0 is a more advanced version of Quinlan's C4.5 classification model that has additional features, such as boosting and unequal costs for different types of errors.
#   - Classification Trees
#        - Combine nonoccurring conditions for splits with several categories
#        - Conduct a final global pruning procedure that attempts to remove the sub-trees with a cost-complexity approach.
#          Here, sub-trees are removed until the error rate exceeds one standard error of the baseline rate (i.e., no pruning).
#   - Classification Rules
#        - When predicting new samples, C5.0 uses all active rules, each of which votes for the most likely class. The votes for each class are
#          weighted by the confidence values and the class associated with the highest vote is used. However, the predicted confidence value is the
#          one associated with the most specific active rule.
#        - The model can create utility bands. Here, the utility is measured as the increase in error that occurs when the rule is removed from the set.
#          The rules are ordered with an iterative algorithm: the model removes the rule with the smallest utility and recomputes the utilities for the other rules.
#          The sequence in which the rules are removed defines their importance.
#          The bands are groups of rule of roughly equal size based on the utility order (highest to smallest).
#          The relationship between the cumulative error rate can be profiled as the groups of rules are added to the model.
#   - Boosting
#        - C5.0 attempts to create trees that are about the same size as the first tree by coercing the trees to have about the same number of terminal nodes
#        - The model combines the predictions from the constituent trees differently than AdaBoost. Each boosted model calculates the confidence values for
#          each class and a simple average of these values is calculated. The class with the largest confidence value is selected.
#        - C5.0 conducts two sorts of "futility analysis" during model training. The model will automatically stop boosting if the model is very effective
#          (i.e., the sum of the weights for the misclassified samples is less than 0.10) or if it is highly ineffective (e.g., the average weight of incorrect
#          samples is greater than 50%). Also, after half of the requested boosting iterations, each sample is assessed to determine if a correct
#          prediction is possible. If it is not, the case is dropped from further computations.
#        - C5.0 uses a different weighting scheme during model training.
#          The weights updating scheme gives a large positive junp in the weights when a sample is incorrectly predicted.
#          When a sample is correctly predicted, the multiplicative nature of the equation makes the weights drop more slowly and with a decreasing rate
#          as the sample continues to be correctly predictd.
#   - Predictor Importance
#        - C5.0 measures predictor importance by determining the percentage of training set samples that fall into all the terminal nodes after the split.
#        - C5.0 also has an option to "winnow" or remove predictors: an initial algorithm uncovers which predictors have a relationship with the outcome,
#          and the final model is created from only the important predictors. To do this, the training set is randomly split in half and a tree is created
#          for the purpose of evaluating the utility of the predictors (call this the "winnowing tree")
#        - Predictors are considered unimportant if they are not in any split in the winnowing tree.
#        - The half of the training set samples not included to create the winnowing tree are used to estimate the error rate of the tree.
#          The error rate is also estimated without each predictor and compared to the error rate when all the predictors are used. If the error rate
#          when all the predictors are used. If the error rate improves without the predictor, it is deemed to be irrelevant and is provisionally removed.
#          Once the tentative list of non-informative predictors is established, C5.0 recreates the tree. If the error rate has become worse, the winnowing process
#          is disabled and no predictors are excluded.
#          After the important predictors are established (if any), the conventional C5.0 training process is used with the full training set but with
#          only the predictors that survived the winnowing procedure.
# ------------------------------------------------------------------------------
library(doMC)
registerDoMC(cores = 20)


c50Grid <- expand.grid(trials = c(1:9, (1:10)*10), model = c("tree", "rules"), winnow = c(TRUE, FALSE))



# ----------
# The nominal C5.0 tree with categorical predictors treated as cohesive sets (grouped categories)
#   - Each categorical predictor is entered into the model as a single entity so that the model decides how to group or split the values
set.seed(476)

c50FactorFit <- train(training[,factorPredictors], training$Class, method = "C5.0", tuneGrid = c50Grid, verbose = FALSE, metric = "ROC", trControl = ctrl)

c50FactorFit

update(plot(c50FactorFit), ylab = "ROC AUC (2008 Hold-Out Data)")

summary(c50FactorFit)



# ----------
# C5.0 models with independent categories:
#   - Categorical predictors decomposed into binary dummy variables to be considered independently forcing binary splits
set.seed(476)

c50Fit <- train(training[,fullSet], training$Class, method = "C5.0", tuneGrid = c50Grid, verbose = FALSE, metric = "ROC", trControl = ctrl)

c50Fit

update(plot(c50Fit), ylab = "ROC AUC (2008 Hold-Out Data)")


# -->
# There was a slight decrease in performance when the winnowing algorithm was applied, although this is likely to be within the
# experimental noize of the data.
# Boosting clearly had a positive effect for these models and there is marginal improvement after about 50 iterations.
# Although single rules did worse than single trees, boosting showed the largest impact on the rule-based models, which ended up having superiror performance.


summary(c50Fit)




# ----------
# confusion matrix and other statistics
confusionMatrix(c50FactorFit, norm = "none")
confusionMatrix(c50Fit, norm = "none")



# ----------
# ROC Curve
c50FactorFit$pred <- merge(c50FactorFit$pred,  c50FactorFit$bestTune)
c50Fit$pred <- merge(c50Fit$pred,  c50Fit$bestTune)


c50Factor_roc <- roc(response = c50FactorFit$pred$obs, predictor = c50FactorFit$pred$successful, levels = rev(levels(c50FactorFit$pred$obs)))
c50_roc <- roc(response = c50Fit$pred$obs, predictor = c50Fit$pred$successful, levels = rev(levels(c50Fit$pred$obs)))


plot(c50_roc, type = "s", print.thres = c(.5), print.thres.pch = 3, print.thres.pattern = "", print.thres.cex = 1.2, col = "red", print.thres.col = "red", legacy.axes = TRUE)
plot(c50Factor_roc, type = "s", print.thres = c(.5), print.thres.pch = 16, print.thres.pattern = "", print.thres.cex = 1.2, add = TRUE, legacy.axes = TRUE)
legend(.75, .2, c("Grouped Categories", "Independent Categories"), lwd = c(1, 1), col = c("black", "red"), pch = c(16, 3))

auc(c50Factor_roc);  auc(c50_roc)
ci(c50Factor_roc);  ci(c50_roc)


# -->
# optimal AUC for independent categories model was 0.94, the best seen among the models


C5imp(c50Fit$finalModel)



# ------------------------------------------------------------------------------
# model by C50 package
# ------------------------------------------------------------------------------
# To obtain boosted versions of C5.0, the traials argumetn is used (with values between 1 and 100)
# By default, the algorithm has internal tests that assess whether the boosting is effective and will halt the model training when it diagnoses that
# it is no longer effective. This feature can be negated using C5.0Control(earlyStopping = FALSE)

C5Boost <- C5.0(Class ~ ., data = training[pre2008,], trials = 10)

C5Boost

summary(C5Boost)

# plot(C5Boost)


C5imp(C5Boost, metric = "usage", pct = TRUE)



# ------------------------------------------------------------------------------
# prediction for hold-out data
# ------------------------------------------------------------------------------
nrow(training[-pre2008,])

c50_ho_pred_prob <- predict(c50Fit, newdata = training[-pre2008, fullSet], type ="prob")
c50_ho_pred <- predict(c50Fit, newdata = training[-pre2008, fullSet], type ="raw")

head(c50_ho_pred_prob, 20)



# ----------
# ROC Curve of predictions for hold-out data
c50_ho_roc <- pROC::roc(response = training[-pre2008, "Class"], predictor = c50_ho_pred_prob[,"successful"], levels = rev(levels(training$Class)))

plot(c50_ho_roc, legacy.axes = TRUE, col = "black")

pROC::auc(c50_ho_roc)



# ------------------------------------------------------------------------------
# diagnose model fit
# ------------------------------------------------------------------------------
testRes_c50 <- data.frame(obs = training[-pre2008, "Class"], c50_ho_pred_prob = c50_ho_pred_prob$successful, c50_ho_pred = c50_ho_pred)



# ----------
# Plot the probability of successful/unsuccessful applications
histogram( ~ c50_ho_pred_prob | obs, data = testRes_c50,
           layout = c(2, 1), nint = 20, xlab = "Probability of Successful Applicationos", type = "count")

