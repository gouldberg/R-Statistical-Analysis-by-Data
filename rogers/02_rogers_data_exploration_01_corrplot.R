setwd("//media//kswada//MyFiles//R//rogers")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rogers
# ------------------------------------------------------------------------------

data("Rogers", package = "MPsychoR")

str(Rogers)

dim(Rogers)

car::some(Rogers)



# ------------------------------------------------------------------------------
# data exploration:  distribution of each variable
# ------------------------------------------------------------------------------

psych::describe(Rogers)


Hmisc::describe(Rogers)



# -->
# each variable has only 4 or 5 values




# ------------------------------------------------------------------------------
# data exploration:  correation anlaysis
# ------------------------------------------------------------------------------

# 26 variables are too large for psych::pairs.panels(Rogers)


# corrplot()
corrplot::corrplot(cor(Rogers), method = "shade", order = "hclust", hclust.method = "ward.D2")


# -->
# we can find some 2 clusters




