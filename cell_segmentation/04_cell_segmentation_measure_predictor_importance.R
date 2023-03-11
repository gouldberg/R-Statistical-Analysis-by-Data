setwd("//media//kswada//MyFiles//R//cell_segmentation")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cell Segmentation in High-Content Screening
# ------------------------------------------------------------------------------
data("segmentationOriginal", package = "AppliedPredictiveModeling")


segTrain <- subset(segmentationData, Case == "Train")
segTest <- subset(segmentationData, Case != "Train")


# ----------
segTrain$Case <- segTrain$Cell <- NULL
segTest$Case <- segTest$Cell <- NULL


# the Class is in the first column
head(names(segTrain))




# ------------------------------------------------------------------------------
# Compute the areas under the ROC curve
# ------------------------------------------------------------------------------
# This is a simple wrapper for the functions roc and auc in the pROC package
# When there are 3 or more classes, filterVarImp will compute ROC curves for each class versus the others and then returns the largest AUC
aucVals <- filterVarImp(x = segTrain[, -1], y = segTrain$Class)

aucVals$Predictor <- rownames(aucVals)

head(aucVals)



# ------------------------------------------------------------------------------
# Calculate the t-test for different means
# ------------------------------------------------------------------------------
# Cacluate the t-tests as before but with x and y switched
segTests <- apply(segTrain[, -1], 2,
                  function(x, y)
                  {
                    tStats <- t.test(x ~ y)[c("statistic", "p.value", "estimate")]
                    unlist(tStats)
                  },
                  y = segTrain$Class)


segTests <- as.data.frame(t(segTests))

names(segTests) <- c("t.Statistic", "t.test_p.value", "mean0", "mean1")
segTests$Predictor <- rownames(segTests)


segTests$difference <- segTests$mean1 - segTests$mean0



# ----------
# Create a volcano plot
# at y-axis, larger for significant difference of means.
xyplot(-log10(t.test_p.value) ~ difference, data = segTests,
       xlab = "Mean With Structure - Mean Without Structure", ylab = "-log(p-Value)", type = "p")




# ------------------------------------------------------------------------------
# Random Forest importance scores
# ------------------------------------------------------------------------------
library(randomForest)

set.seed(791)

rfImp <- randomForest(Class ~ ., data = segTrain, ntree = 2000, importance = TRUE)

rfValues <- data.frame(RF = importance(rfImp)[, "MeanDecreaseGini"], Predictor = rownames(importance(rfImp)))



# ------------------------------------------------------------------------------
# The Relief value
#   - The Relief algorithm (Kira and Rendell 1992) is a generic method for quantifying predictor importance.
#     It was originally developed for classification problems with two classes but has been extended to work across a wider range of problems
#   - It can accomodate continuous predictors as well as dummy variables and can recognize nonlinear relationships between the predictors and the outcome.
#   - It uses random selected points and their nearest neighbors to evaluate each predictor in isolation.
#     For a particular predictor, the score attempts to characterize the separation between the classes in isolated sections of the data.
#     For a randomly selected training set sample, the algorithm finds the nearest samples from both classes (called the "hits" and "misses").
#     For each predictor, a measure of difference in the predictor's values is calculated between the random data point and the hits and misses.
#     The idea is that a predictor that shows a separation between the classes should have hits nearby and missed far away.
#     Larger scores are indicative of important predictors.
#   - For continuous predictors, Kira nad Rendell (1992) suggested the distance between the two points be divided by the overall range of the predictor
#       diff(x, y) = (x - y) / C   where C is a constant for the predictor that scales the difference to be between 0 and 1.
#     For binary data, a simple indicator for equivalence can be used:  diff(x, y) = | x - y |
#
#   - Kononenko (1994) modified algorithm, called ReliefF, to use more than a single nearest neighbors, use a modified difference metric, 
#     and allow for more than two classes as well as missing predictor values. Additionally, Robnik-Sionja and Kononenko (1997) adapted the 
#     algorithm for regression (i.e., numeric outcomes)
# ------------------------------------------------------------------------------
library(CORElearn)


# estimator = "RReliefFequalK":  k nearest instances have equal weight
# This function can also be used to calculate the gain ratio, Gini indx, and other scores.
set.seed(791)

ReliefValues <- attrEval(Class ~ ., data = segTrain, estimator="ReliefFequalK", ReliefIterations = 50)

ReliefValues %>% sort(., decreasing = TRUE)

ReliefValues <- data.frame(Relief = ReliefValues, Predictor = names(ReliefValues))



# ----------
# Permutation approach to investigate the observed values of the ReliefF statistic
# Remove 2 variables since all values of those variables in a data split are equal
tmp <- segTrain %>% dplyr::select(-Class, -MemberAvgTotalIntenStatusCh2, -MemberAvgAvgIntenStatusCh2)

perm <- permuteRelief(x = tmp, y = ifelse(segTrain$Class == "PS", 1, 0),
                      nperm = 500, estimator = "RReliefFequalK", ReliefIterations = 50)


# permutated ReliefF scores
perm$permutations



# ----------
# permutation distributions
histogram(~ value | Predictor, data = perm$permutations)



# ----------
# also the standardized versions of the scores
# to represent the number of standard deviations that the observed ReliefF values (without permuting)
# are from the center of the permuted distribution

perm$standardized



# ------------------------------------------------------------------------------
# The Maximal Information Coefficient (MIC)
#   - Reshef et al. (2011) describe a new metric to quantify the relationship between two vairables called the MIC.
#     Their method partitions the two-dimensional area defined by the predictor and outcome into sets of two-dimensional grids.
#     Within each grid, the number of data points is calculated and used to compute the mutual information statistic (Brillinger 2004),
#     which is related to the information criteria for C4.5 and C5.0 decision trees.
#     Many different configurations of the same grid size are evaluated and the largest mutual information value is determined.
#     This process is repeated for many different grid sizes. The compendium of mutual information values across all the bins are normalized
#     and the largest value across all the bin configurations is used as the strength of association between the predictor and the outcome.
#   - Authors demonstrate that this method can detect many types of relationships, such as sin waves, ellipses, and other highly nonlinear patterns.
#     One potential issue with such a general technique is that it may not do as well as others under some circumstances.
#     For example, if the true relationship between the predictor and outcome were linear, the simple correlation statistic may perform better.
# ------------------------------------------------------------------------------

# mine computes the MINE (Maximal Information-Based Nonparametric Exploration) family measures between two variables
set.seed(791)

mine_stats <- minerva::mine(x = segTrain[, -1], y = ifelse(segTrain$Class == "PS", 1, 0))

mine_stats

segMIC <- mine_stats$MIC

segMIC <- data.frame(Predictor = names(segTrain[,-1]), MIC = segMIC[,1])

segMIC



# ------------------------------------------------------------------------------
# Combine all stats for predictor importance
# ------------------------------------------------------------------------------
rankings <- merge(segMIC, ReliefValues)
rankings <- merge(rankings, rfValues)
rankings <- merge(rankings, segTests)
rankings <- merge(rankings, aucVals)
rankings

rankings$channel <- "Channel 1"
rankings$channel[grepl("Ch2$", rankings$Predictor)] <- "Channel 2"
rankings$channel[grepl("Ch3$", rankings$Predictor)] <- "Channel 3"
rankings$channel[grepl("Ch4$", rankings$Predictor)] <- "Channel 4"
rankings$t.Statistic <- abs(rankings$t.Statistic)


splom(~rankings[, c("PS", "t.Statistic", "RF", "Relief", "MIC")],
      groups = rankings$channel,
      varnames = c("ROC\nAUC", "Abs\nt-Stat", "Random\nForest", "Relief", "MIC"),
      auto.key = list(columns = 2))


# -->
# MIC and Relief scores are roughly concordant.
# AUC shows a tight, curvlinear relationship with the random forest score and a strong linear relationship with the t-statistic.
# The t-statistic and random forest appear to rank the same predictors as less important, but there are differences in the scores for those quantified
# as most important.

