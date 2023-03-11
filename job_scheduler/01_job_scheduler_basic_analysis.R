rm(list=ls())

setwd("//media//kswada//MyFiles//R//job_scheduler")

packages <- c("dplyr", "caret", "AppliedPredictiveModeling", "lattice", "e1071")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Job Scheduler
#   - Pfizer runs a large number of jobs in its High-Performance Computing (HPC) environment. One class of jobs for characterizing compounds is run regularly,
#     some of which can be computationally burdensome. Over a period of time, resource utilization information was logged about this class of jobs.
#     Additionally, there are several task characteristics that can be collected at the time of job launch:
#       - Protocal / Number of compounds / Number of input fields / Number of iterations / Pending job count / Time of day / Day of the week
#   - Execution time was recorded for each job. Time spent in pending or suspended states were not counted in these values.
#     Jobs were required to be classified as either very fast (1m or less), fast (1-50m), moderate (5-30m), or long (greater than 30m).
#     Most of the jobs fall into either the very fast category (51.1%) or the fast category (31.1%) while only 11.9% were classified as moderate and 6% were long.
#   - The goal of this experiment is to predict the class of the jobs. The types of errors are not equal and there should be a greater penalty for
#     classifying jobs as very short that are in fact long. Also, since the prediction equation will need to be implemented in software and quickly
#     computed, models with simpler prediction equations are preferred.
#   - The efficiency of the scheduler can be significantly affected by the amount and quality of job information that is knowb at the time of submission.
# ------------------------------------------------------------------------------
data(schedulingData)


str(schedulingData)



# ----------
# Make a vector of predictor names
predictors <- names(schedulingData)[!(names(schedulingData) %in% c("Class"))]

predictors_cont <- c("Compounds", "InputFields", "Iterations", "NumPending")



# ------------------------------------------------------------------------------
# summary of the data
# ------------------------------------------------------------------------------
library(Hmisc)
describe(schedulingData)




# ------------------------------------------------------------------------------
# Basic checks for predictors:  skewness
# ------------------------------------------------------------------------------
apply(schedulingData[, predictors_cont], MARGIN=2, FUN=summary)

count_zero <- function(x) sum(x == 0)
apply(schedulingData[, predictors_cont], MARGIN=2, FUN=count_zero)



# ----------
# skewness
round(apply(schedulingData[, predictors_cont], MARGIN=2, FUN=skewness), digits = 3)



