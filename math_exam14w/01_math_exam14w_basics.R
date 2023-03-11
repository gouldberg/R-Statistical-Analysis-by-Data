setwd("//media//kswada//MyFiles//R//youthdep")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MathExam14W
#   - Mathematics exam dataset.
#     We use following 7 cavoriates, subject to DIF analysis
#       - nsolved:  number of items solved
#       - tests:  number of online test exercises
#       - gender:  gender
#       - study:  two types of degrees
#       - semester:  semester enrolled
#       - attempt:  number of times the exam has been attempted
#       - group:  whether the students were in the first or second batch
# ------------------------------------------------------------------------------

data("MathExam14W", package = "psychotools")


str(MathExam14W)

dim(MathExam14W)


head(MathExam14W)



# ----------
itmath <- as.list.data.frame(MathExam14W$solved)

covars <- MathExam14W[,3:9]

car::some(covars)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------



