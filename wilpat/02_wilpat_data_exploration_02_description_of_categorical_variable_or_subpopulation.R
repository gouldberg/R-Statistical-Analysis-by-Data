setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")

str(WilPat)



# ----------
# Let us pick six items: gay marriage, sexual freedom, gay adoption, gender quotas, affirmative action, and legalized marijuana
# and country variable (Hungary, the USA, and India)
WP6 <- WilPat[, c(32, 38, 41, 44, 45, 46, 47)]


# In addition, include 4 more variables:
#   - self-reported liberal-consevative item (re-categorized into 4 categories)  (we will take as ordinal)
#   - self-reported political lef-right identification item on a 10-point scale  (we use a spline transformation since there are many categories)
#   - gender (nominal) and age (linear)
WPmixed <- WilPat[, c(32, 38, 41, 44, 45, 46, 47:51)]


WPmixed <- rapply(WPmixed, f = as.factor, classes = "numeric", how = "replace")



# ------------------------------------------------------------------------------
# Description of a categorical variable by a categorical variable
# ------------------------------------------------------------------------------

library(FactoMineR)


# num.var = 1:  "GayMarriage"
wp.catd <- catdes(WPmixed, num.var = 1)


wp.catd



# ----------
# X^2 test
wp.catd$test.chi


# -->
# The smaller the p-value associated with the X^2 test, the more questionable the independence hypothesis.
# and the more the categorical varaible characterises the variable "GayMarriage".

# the variable "GayAdoption" is the most closely related to the variable "GayMarriage".



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

