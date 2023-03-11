setwd("//media//kswada//MyFiles//R//ucb_admissions")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UCBAdmissions
# ------------------------------------------------------------------------------

data("UCBAdmissions", package = "datasets")


data <- UCBAdmissions


data


dimnames(data)


dim(data)




# ------------------------------------------------------------------------------
# Visualize relationships in mutiway frequency tables by doubledecker()
#  - doubledecker plots visualize the dependence of one categorical (typically binary) variable on further categorical variables
#  - Formally, they are mosaic plots with vertical splits for all dimensions (predictors) except the last one,
#    which represents the dependent variable (outcome)
#  - The last variable is visualized by horizontal splits, no space between the tiles, and separate colors for the levels
#  - This plot have the advantage of making it easier to "read" the differences among the conditional response proportions
#    in relation to combinations of the explanatory variables
#  - Moreover, for a binary response, the difference in these conditional proportions for any two columns has a direct relation
#    to the odds ratio for a positive response in relation to those predictor levels.
# ------------------------------------------------------------------------------

library(vcd)


# By default, the levels of the response (Admit) are taken in their order in the array and shaded to highlight the last level (Rejected)
# We want to highlight Admitted, so we reverse this dimension


doubledecker(Admit ~ Gender, data = data)


doubledecker(Admit ~ Gender, data = data[2:1,,])


doubledecker(Admit ~ Dept + Gender, data = data[2:1,,])




# -->
# It is easy to see the effects of both Dept adn Gender on Admit.
# Admission rate declines across departments A-E, and within departments,
# the proportion admitted is roughly the same, except for department A, where more female applicatns are admitted.


