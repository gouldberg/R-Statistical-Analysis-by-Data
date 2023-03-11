setwd("//media//kswada//MyFiles//R//tea")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tea
#  - survey of 300 tea drinkers for tea-drinking behaviour (19 questions), the image they have of the product (12 questions) and 4 descriptive quesionts
# ------------------------------------------------------------------------------

tea <- read.table("tea.csv", header = TRUE, sep = ";")

dim(tea)

str(tea)


car::some(tea)



# ------------------------------------------------------------------------------
# Description of a categorical variable by a categorical variable
# ------------------------------------------------------------------------------

library(FactoMineR)


# num.var = 18:  "type"
tea.catd <- catdes(tea, num.var = 18)


tea.catd



# ----------
# X^2 test
tea.catd$test.chi


# -->
# The smaller the p-value associated with the X^2 test, the more questionable the independence hypothesis.
# and the more the categorical varaible characterises the variable "type".

# the variable "place of purchase" is the most closely related to the variable "type".



# ------------------------------------------------------------------------------
# Close investigation
# ------------------------------------------------------------------------------

( tab <- xtabs(~ type + place.of.purchase, data = tea) )


mosaic(tab, gp = shading_Friendly2, gp_arg = list(interpolate = 1:4), labeling = labeling_residuals)


# -->
# Closely look more at specialist shop



# ----------
# Description of the Category "luxury" of the variable "Type" by the categories of the categorical variables
tea.catd$category$luxury

addmargins(tab)


# -->
# The categories of all the categorical variables are organised from most to least characteristic
# when the category is overrepresented in the given class compared to the other categories (v-test is therefore positive),
# and from least characteristic to most when the category is underrepresented in the class (v-test is negative),

# The individuals who buy luxury tea are most significantly characterised by the fact that they do not buy tea in supermarkets
# (v-test is negative and highest absolute value)

# 70% (= 21 / 30) of the individuals who buy their tea in specialist shops also belong to the class "luxury"
# 39.6% (= 21 / 53) of the individuals from the class "luxury" purchase their tea in specialist shops
# 10% (= 30 / 300) of the participants purchase their tea in specialist shops

# The p-value of the v-test is provided along with the associated v-test
# v-test null hypothesis:
# "the values of X for the individuals who chose the category q are selected at random from all of the possible values of X."

