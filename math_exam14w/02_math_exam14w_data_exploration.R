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
# data preparation:  convert to item response data
# ------------------------------------------------------------------------------

library(psychotree)


# Note that item responses, in order to be processed by the raschtree function, 
# have to be provided as object of class "itemresp"

mex <- data.frame(solved = itemresp(itmath), covars)



# ----------
head(mex, 5)

itmath[1:5,]



# -->
# mex:  itmath(item response data) + covars



# ----------
mscale(mex$solved)




# ------------------------------------------------------------------------------
# data preparation:  eliminate persons with 0 / full responses
# ------------------------------------------------------------------------------

mex <- subset(mex, nsolved > 0 & nsolved < 13)


graphics.off()
plot(mex$solved)




# ------------------------------------------------------------------------------
# data preparation:  define the factor levels for the covariates properly
# ------------------------------------------------------------------------------

mex$tests <- ordered(mex$tests)


mex$nsolved <- ordered(mex$nsolved)


mex$attempt <- ordered(mex$attempt)


