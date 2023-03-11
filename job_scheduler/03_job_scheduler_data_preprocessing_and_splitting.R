setwd("//media//kswada//MyFiles//R//job_scheduler")

packages <- c("dplyr", "caret", "AppliedPredictiveModeling", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Job Scheduler
# ------------------------------------------------------------------------------
data(schedulingData)


str(schedulingData)



# ----------
# Make a vector of predictor names
predictors <- names(schedulingData)[!(names(schedulingData) %in% c("Class"))]



# ------------------------------------------------------------------------------
# data splitting and preprocessing
# ------------------------------------------------------------------------------
set.seed(1104)

inTrain <- createDataPartition(schedulingData$Class, p = .8, list = FALSE)



# ----------
# There are a lot of zeros and the distribution is skewed. We add one so that we can log transform the data
schedulingData$NumPending <- schedulingData$NumPending + 1

trainData <- schedulingData[ inTrain,]
testData  <- schedulingData[-inTrain,]



# ------------------------------------------------------------------------------
# Create model formula
# ------------------------------------------------------------------------------
# Create a main effects only model formula to use repeatedly. Another formula with nonlinear effects is created below.
modForm <- as.formula(Class ~ Protocol + log10(Compounds) +
                        log10(InputFields)+ log10(Iterations) +
                        log10(NumPending) + Hour + Day)



# ----------
# Create an expanded set of predictors with interactions. 
modForm2 <- as.formula(Class ~ (Protocol + log10(Compounds) +
                                  log10(InputFields)+ log10(Iterations) +
                                  log10(NumPending) + Hour + Day)^2)



# ------------------------------------------------------------------------------
# remove zero variance predictor
# ------------------------------------------------------------------------------
# Some of these terms will not be estimable.
# For example, if there are no data points were a particular protocol was run on a particular day, the full interaction cannot be computed.
# We use model.matrix() to create the whole set of predictor columns, then remove those that are zero variance

expandedTrain <- model.matrix(modForm2, data = trainData)
expandedTrain <- as.data.frame(expandedTrain)

expandedTest  <- model.matrix(modForm2, data = testData)
expandedTest  <-  as.data.frame(expandedTest)



# ----------
# Some models have issues when there is a zero variance predictor within the data of a particular class,
# so we used caret's checkConditionalX() function to find the offending columns and remove them
zv <- checkConditionalX(expandedTrain, trainData$Class)
zv



# ----------
# Keep the expanded set to use for models where we must manually add more complex terms (such as logistic regression)
expandedTrain <-  expandedTrain[,-zv]
expandedTest  <-  expandedTest[, -zv]

