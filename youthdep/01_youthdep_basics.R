setwd("//media//kswada//MyFiles//R//youthdep")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  YouthDep
#   - dataset from Vaughn-Coaxum et al. (2016) who used the children's depression inventory (CDI) that rates the severity of symptoms related
#     to depression children and adolescents.
# ------------------------------------------------------------------------------

data("YouthDep", package = "MPsychoR")

str(YouthDep)

dim(YouthDep)


head(YouthDep)



# ----------
# We pick two items
#   - "I am sad all the time" (item1) and "I look ugly" (item2)
#   - The items are on a three-point scale with values 0, 1, and 2.

# And for simplicity, we dichotomize the items by merging categories 1 and 2 into a single category.
item1 <- YouthDep[, 1]
levels(item1) <- c("0", "1", "1")

item2 <- YouthDep[,14]
levels(item2) <- c("0", "1", "1")



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

table(item1, item2)



