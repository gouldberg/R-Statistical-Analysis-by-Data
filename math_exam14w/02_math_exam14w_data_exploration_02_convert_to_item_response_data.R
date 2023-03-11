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



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


summary(covars)




# itmath to be converted to item response data.
# For RaschTree model, the response should be dichotomous data (0 or 1)


str(itmath)










