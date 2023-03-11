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
# test parameter instability at each node
# ------------------------------------------------------------------------------

library("strucchange")


# sctest(mrt, node = 3)


sctest(mrt, node = 4)


# sctest(mrt, node = 6)


sctest(mrt, node = 7)



# -->
# node 4 and 7 is OK for parameter stability

