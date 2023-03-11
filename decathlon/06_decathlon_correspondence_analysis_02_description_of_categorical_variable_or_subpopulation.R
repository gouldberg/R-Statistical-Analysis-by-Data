setwd("//media//kswada//MyFiles//R//decathlon")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  decathlon
# ------------------------------------------------------------------------------

data("decathlon", package = "FactoMineR")

str(decathlon)

dim(decathlon)


decathlon



# ------------------------------------------------------------------------------
# Description of a categorical variable by a categorical variable
# ------------------------------------------------------------------------------

library(FactoMineR)


# num.var = 11 here:  Competition
dec.catd <- catdes(decathlon[,c(1:10,13)], num.var = 11)


dec.catd



# ----------
# X^2 test
dec.catd$test.chi


# -->
# The smaller the p-value associated with the X^2 test, the more questionable the independence hypothesis.
# and the more the categorical varaible characterises the variable "Competition".

# the variable "100m" is the most closely related to the variable "Competition".



# ------------------------------------------------------------------------------
# Close investigation
# ------------------------------------------------------------------------------

( tab <- xtabs(~ J31 + J57, data = perfume) )


mosaic(tab, gp = shading_Friendly2, gp_arg = list(interpolate = 1:4), labeling = labeling_residuals)


# -->
# Closely look more at Green



# ----------
# Description of the Category "J31" of the variable "J57" by the categories of the categorical variables
perf.catd$category$soft

addmargins(tab)


