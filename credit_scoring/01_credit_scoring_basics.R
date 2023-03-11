setwd("//media//kswada//MyFiles//R//credit_scoring")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "e1071")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Credit Scoring
#   - The German credit data set is a popular tool for benchmarking machine learning algorithms.
#     It contains 1,000 samples that have been given labels of good and bad credit.
#     In the data set, 70% were rated as having good credit.
# ------------------------------------------------------------------------------
data("GermanCredit", package = "caret")

data <- GermanCredit

dim(data)

str(data)

table(data$Class)



# ------------------------------------------------------------------------------
# Check the variables with near-zero variance
# ------------------------------------------------------------------------------
nzv <- nearZeroVar(data)

length(nzv)

apply(data[, nzv], MARGIN=2, table)

# Hmisc::describe(data)



# ------------------------------------------------------------------------------
# Check skewness of integer variables
# ------------------------------------------------------------------------------
col_int <- unlist(lapply(data[,names(data)!="Class"], is.integer)) 

round(apply(data[,col_int], MARGIN = 2, FUN = skewness), digits = 3)


