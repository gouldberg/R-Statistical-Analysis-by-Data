rm(list=ls())

setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "minerva", "CORElearn")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  solubility
# ------------------------------------------------------------------------------
data("solubility", package = "AppliedPredictiveModeling")

ls(pattern = "^solT")


dim(solTrainX)

names(solTrainX)

head(solTestY)


str(solTrainX)



# ----------
# keep the continuous predictors and append the outcome to the data frame
SolContPred <- solTrainXtrans[, !grepl("FP", names(solTrainXtrans))]
numSolPred <- ncol(SolContPred)
SolContPred$Sol <- solTrainY



# ----------
# keep the categorical predictors
SolCatPred <- solTrainXtrans[, grepl("FP", names(solTrainXtrans))]
SolCatPred$Sol <- solTrainY
numSolCatPred <- ncol(SolCatPred) - 1



# ------------------------------------------------------------------------------
# Estimate the correlations between the numeric predictors and continuous outcomes
# ------------------------------------------------------------------------------
# Determine which columns have the string "FP" in the name and exclude these to get the numeric predictors
fpCols <- grepl("FP", names(solTrainXtrans))
numericPreds <- names(solTrainXtrans[!fpCols])



# ----------
# Pearson and Spearman's rank correlation for all numeric variables
corrValues <- apply(solTrainXtrans[, numericPreds], MARGIN = 2, FUN = function(x, y) cor(x, y), y = solTrainY)
corrValues_rank <- apply(solTrainXtrans[, numericPreds], MARGIN = 2, FUN = function(x, y) cor(x, y, method = "spearman"), y = solTrainY)


corrValues %>% sort(., decreasing = TRUE)
corrValues_rank %>% sort(., decreasing = TRUE)



# ----------
# Calculate the correlation matrices and keep the columns with the correlations between the predictors and the outcome
correlations <- cor(SolContPred)[-(numSolPred+1),(numSolPred+1)]

rankCorrelations <- cor(SolContPred, method = "spearman")[-(numSolPred+1),(numSolPred+1)]

corrs <- data.frame(Predictor = names(SolContPred)[1:numSolPred], Correlation = correlations, RankCorrelation  = rankCorrelations)



# ------------------------------------------------------------------------------
# LOESS smoother and the summary measure by loess
# ------------------------------------------------------------------------------
# a pseudo-R^2 statistic is calculated
smoother <- loess(solTrainY ~ solTrainXtrans$NumCarbon)

smoother

xyplot(solTrainY ~ solTrainXtrans$NumCarbon, type = c("p", "smooth"), xlab = "# Carbons", ylab = "Solubility")

featurePlot(solTrainXtrans[, c("NumCarbon", "SurfaceArea2")], solTrainY, between = list(x = 1), type = c("g", "p", "smooth"),
            df = 3, aspect = 1, labels = c("", "Solubility"))



# ----------
# The caret function filterVarImp with the nonpara = TRUE option (for nonparametric regression) creates a LOESS model for each predictor
# and quantifies the relationship with the outcome.
smoother <- filterVarImp(x = SolContPred[, -ncol(SolContPred)], y = solTrainY, nonpara = TRUE)
smoother$Predictor <- rownames(smoother)
names(smoother)[1] <- "Smoother"

smoother



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
mine_stats <- minerva::mine(x = SolContPred[, 1:numSolPred], y = solTrainY)

mine_stats

MIC <- mine_stats$MIC

MIC <- data.frame(Predictor = names(SolContPred)[1:numSolPred], MIC = MIC)

MIC



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
ReliefF <- attrEval(Sol ~ .,  data = SolContPred, estimator = "RReliefFequalK", ReliefIterations = 50)

ReliefF %>% sort(., decreasing=TRUE)

ReliefF <- data.frame(Predictor = names(ReliefF), Relief = ReliefF)



# ----------
# Permutation approach to investigate the observed values of the ReliefF statistic
perm <- permuteRelief(x = solTrainXtrans[, !grepl("FP", names(solTrainXtrans))], 
                      y = solTrainY, nperm = 500, estimator = "RReliefFequalK", ReliefIterations = 50)


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
# Compare scores for each variable importane
#  - smoother:  psuedo-R^2
#  - Spearman's Rank Correlation
#  - Maximal Information Coefficient
#  - Relief
# ------------------------------------------------------------------------------
# Combine all scores
contDescrScores <- merge(smoother, corrs)
contDescrScores <- merge(contDescrScores, MIC)
contDescrScores <- merge(contDescrScores, ReliefF)

rownames(contDescrScores) <- contDescrScores$Predictor

contDescrScores



# ----------
contDescrSplomData <- contDescrScores
contDescrSplomData$Correlation <- abs(contDescrSplomData$Correlation)
contDescrSplomData$RankCorrelation <- abs(contDescrSplomData$RankCorrelation)
contDescrSplomData$Group <- "Other"
contDescrSplomData$Group[grepl("Surface", contDescrSplomData$Predictor)] <- "SA"


splom( ~ contDescrSplomData[,c(3, 4, 2, 5)], groups = contDescrSplomData$Group, varnames = c("Correlation", "Rank\nCorrelation", "LOESS", "MIC"))



# ------------------------------------------------------------------------------
# For categorical preddictors, t-test for different means
# ------------------------------------------------------------------------------
tests <- apply(SolCatPred[, 1:numSolCatPred], 2,
               function(x, y)
               {
                 tStats <- t.test(y ~ x)[c("statistic", "p.value", "estimate")]
                 unlist(tStats)
               },
               y = solTrainY)



# The results are a matrix with predictors in columns. We reverse this
tests <- as.data.frame(t(tests))
names(tests) <- c("t.Statistic", "t.test_p.value", "mean0", "mean1")
tests$difference <- tests$mean1 - tests$mean0
tests



# ----------
# Create a volcano plot
# at y-axis, larger for significant difference of means.
xyplot(-log10(t.test_p.value) ~ difference, data = tests,
       xlab = "Mean With Structure - Mean Without Structure", ylab = "-log(p-Value)", type = "p")

