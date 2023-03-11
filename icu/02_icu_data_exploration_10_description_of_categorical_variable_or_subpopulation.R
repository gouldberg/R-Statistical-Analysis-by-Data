setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ------------------------------------------------------------------------------
# Description of a categorical variable by a categorical variable
# ------------------------------------------------------------------------------

library(FactoMineR)


# num.var = 1:  "died"
icu.catd <- catdes(died, num.var = 1)


icu.catd



# ----------
# X^2 test
icu.catd$test.chi


# -->
# The smaller the p-value associated with the X^2 test, the more questionable the independence hypothesis.
# and the more the categorical varaible characterises the variable "died".

# the variable "uncons" is the most closely related to the variable "died".



# ------------------------------------------------------------------------------
# Close investigation
# ------------------------------------------------------------------------------

( tab <- xtabs(~ GayMarriage + GayAdoption, data = WPmixed) )


library(vcd);  library(vcdExtra);

mosaic(tab, gp = shading_Friendly2, gp_arg = list(interpolate = 1:4), labeling = labeling_residuals)


# -->
# Closely look more at specialist shop



# ----------
# Description of the Category "0" (approve) of the variable "GaryMarriage" by the categories of the categorical variables
wp.catd$category$'0'

addmargins(tab)


# -->
# The categories of all the categorical variables are organised from most to least characteristic
# when the category is overrepresented in the given class compared to the other categories (v-test is therefore positive),
# and from least characteristic to most when the category is underrepresented in the class (v-test is negative),

# The p-value of the v-test is provided along with the associated v-test
# v-test null hypothesis:
# "the values of X for the individuals who chose the category q are selected at random from all of the possible values of X."

