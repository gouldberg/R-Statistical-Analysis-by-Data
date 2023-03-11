setwd("//media//kswada//MyFiles//R//punishment")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Punishment
# ------------------------------------------------------------------------------

data("Punishment", package = "vcd")


data <- Punishment


data


str(Punishment)



# ----------
# convert data.frame to table

tab <- xtabs(Freq ~ memory + attitude + age + education, data = data)


dimnames(tab) <- list(
  Memory = c("yes", "no"),
  Attitude = c("no", "moderate"),
  Age = c("15-24", "25-39", "40+"),
  Education = c("Elementary", "Secondary", "High")
)



# Memory * Attitude * Age * Education
tab




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


doubledecker(Attitude ~ Memory, data = tab)


doubledecker(Attitude ~ Memory + Age, data = tab)


doubledecker(Attitude ~ Memory + Education, data = tab)




# ----------
doubledecker(Memory ~ Education + Age, data = tab)


doubledecker(Memory ~ Age + Education, data = tab)




# ----------
doubledecker(Attitude ~ Education + Age, data = tab)


doubledecker(Attitude ~ Age + Education, data = tab)




