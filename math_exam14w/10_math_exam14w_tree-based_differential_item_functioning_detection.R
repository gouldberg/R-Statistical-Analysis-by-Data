setwd("//media//kswada//MyFiles//R//youthdep")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MathExam14W
# ------------------------------------------------------------------------------

data("MathExam14W", package = "psychotools")


str(MathExam14W)

dim(MathExam14W)


head(MathExam14W)



# ----------
itmath <- as.list.data.frame(MathExam14W$solved)

covars <- MathExam14W[,3:9]

car::some(covars)



# ----------
library(psychotree)

mex <- data.frame(solved = itemresp(itmath), covars)
mex <- subset(mex, nsolved > 0 & nsolved < 13)

mex$tests <- ordered(mex$tests)
mex$nsolved <- ordered(mex$nsolved)
mex$attempt <- ordered(mex$attempt)




# ------------------------------------------------------------------------------
# Tree-Based DIF detection
#   - Based on multiple external metric or categorical variables, the algorithm tries to find splits for which the item parameters differ.
#     If no split is found, there is no DIF.
# ------------------------------------------------------------------------------

set.seed(1)


mrt <- raschtree(solved ~ group + tests + nsolved + gender +
                   attempt + study + semester, 
                 data = mex, vcov = "info", minsize = 50, ordinal = "l2", nrep = 1e5)



# ----------
plot(mrt)
# plot(mrt, type = "region")




# -->
# The first split is according to the student group variable (there were two batches of students)
# Students in the first batch were again split according to the test variable
# (the number of online test exercises solved correctly prior to the written exam)
# Note that the algorithm splits this metric variable at a value of 16.
# Depending on whether batch 1 students solved more or less than 16 exercises correctly, the item parameters differ.

# For the strudents in batch 2, a splt according to the student's raw score was obtained
# (four or less items correct vs. more than four items correct).
# The item parameter profiles (items on the x-axis, difficulty parameters on the y-axis) are given at the bottom



# ----------
# item difficulty parameters for each node
round(itempar(mrt), 2)



# -->
# node 7:  "solvedpayflow" = 2.92
# but "solvedintegral" = 0.31


