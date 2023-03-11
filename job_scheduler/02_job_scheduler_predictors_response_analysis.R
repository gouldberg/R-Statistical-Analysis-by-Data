rm(list=ls())

setwd("//media//kswada//MyFiles//R//job_scheduler")

packages <- c("dplyr", "caret", "AppliedPredictiveModeling", "lattice")
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



# ------------------------------------------------------------------------------
# summary of the data
# ------------------------------------------------------------------------------
library(Hmisc)
describe(schedulingData)




# ------------------------------------------------------------------------------
# summary of the data
# ------------------------------------------------------------------------------
# tabplot is not available for R 3.5.2
library(tabplot)
tableplot(schedulingData[, c( "Class", predictors)])



# ------------------------------------------------------------------------------
# Protocal vs. Class
# ------------------------------------------------------------------------------
library(vcd)
tab <- xtabs(~ Protocol + Class, data = schedulingData)
tab

round(prop.table(tab, margin = 1), digits = 3)

vcd::mosaic(tab, gp = shading_Friendly)


# -->
# 20.1% of protocol "O" is Class == "L"
# Other than "O", Protocol "A", "C"", "H" has more than 10% for Class = "L" 
# Protocol "D", E" and "K" does not have Class == "L"

# Given these relationships, we might expect the protocol information to be potentially important



# ------------------------------------------------------------------------------
# Compounds (continuous) vs. Class
# Input Fields (continuous) vs. Class 
# NumPending (continuous) vs. Class 
# Iterations (continuou) vs. Class
# ------------------------------------------------------------------------------
bwplot(Class ~ Compounds, data = schedulingData, horizontal = TRUE)
bwplot(Class ~ InputFields, data = schedulingData, horizontal = TRUE)
bwplot(Class ~ NumPending, data = schedulingData, horizontal = TRUE)
bwplot(Class ~ Iterations, data = schedulingData, horizontal = TRUE)


# -->
# The jobs associated with a large number of compounds tend to be eigher large or moderate in execution time.


histogram(~ Compounds | Class, data = schedulingData)
histogram(~ InputFields | Class, data = schedulingData)
histogram(~ NumPending | Class, data = schedulingData)
histogram(~ Iterations | Class, data = schedulingData)


tmp <- schedulingData %>% mutate(numpen_cat = cut(NumPending, 5))
tab <- xtabs(~ Class + numpen_cat, data = tmp)
tab

vcd::mosaic(tab, gp = shading_Friendly)


# -->
# Many of the jobs of moderate length were submitted when the number of pending jobs was very high.
# However, this trend does no reproduce itself in the very long jobs.


# When the number of iterations is large, the job tends to go long.



# ------------------------------------------------------------------------------
# Scatter plot matrix
# Continuous variable by Category Variable and Class
# Relationship between Compounds and InputFields by Protocol and Class
# ------------------------------------------------------------------------------
library(lattice)

xyplot(Compounds ~ InputFields | Protocol,
       data = schedulingData, scales = list(x = list(log = 10), y = list(log = 10)),
       groups = Class, xlab = "Input Fields", auto.key = list(columns = 4), aspect = 1, as.table = TRUE)


# --> 
# For some cases, such as protocols A, C, D, H, I and K, the number of compounds and fields appears to provide information to differentiate the classes.
# However, these relationships are class specific; the patterns for protocols I and K are different.
# The correlation pattern is protocol-specific.



# ------------------------------------------------------------------------------
# Week Day (category)  vs. Class
# ------------------------------------------------------------------------------
tab <- xtabs(~ Day + Class, data = schedulingData)
tab

round(prop.table(tab, margin = 1), digits = 3)

vcd::mosaic(tab, gp = shading_Friendly)


# -->
# Tuesday:  "M" is large
# Saturday:  Number of jobs are very small but "L" is large



# ------------------------------------------------------------------------------
# Hour (continuous) vs. Class
# ------------------------------------------------------------------------------
histogram(~ Hour, data = schedulingData, breaks = seq(0,24,by=0.25))


# -->
# The time of day (Eastern Standard Time) that the job was launched (0 to 24)
# The distribution is multimodal, which reflects users coming online in 3 different time zones on two continents.


