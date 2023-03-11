setwd("//media//kswada//MyFiles//R//gmo")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gmo
# ------------------------------------------------------------------------------

gmo <- read.table("gmo.csv", header = TRUE, sep = ";", dec = ".")

dim(gmo)

str(gmo)

car::some(gmo)


# ----------
levels(gmo$Position.Al.H)[4] <- levels(gmo$Position.Al.H)[1]

levels(gmo$Position.Culture) <- c("Favourable", "Somewhat Against", "Totally opposed", "Favourable")



# ------------------------------------------------------------------------------
# Description of a categorical variable by a categorical variable
# ------------------------------------------------------------------------------

library(FactoMineR)


# num.var = 21:  "Political.Party"
gmo.catd <- catdes(gmo, num.var = 21)


gmo.catd



# ----------
# X^2 test
gmo.catd$test.chi


# -->
# The smaller the p-value associated with the X^2 test, the more questionable the independence hypothesis.
# and the more the categorical varaible characterises the variable "Political.Party".

# the variable "Position.Culture" is the most closely related to the variable "Political.Party".



# ------------------------------------------------------------------------------
# Close investigation
# ------------------------------------------------------------------------------

( tab <- xtabs(~ Political.Party + Position.Culture, data = gmo) )


mosaic(tab, gp = shading_Friendly2, gp_arg = list(interpolate = 1:4), labeling = labeling_residuals)


# -->
# Closely look more at Green



# ----------
# Description of the Category "Green" of the variable "Political.Party" by the categories of the categorical variables
gmo.catd$category$Green

addmargins(tab)


# -->
# The categories of all the categorical variables are organised from most to least characteristic
# when the category is overrepresented in the given class compared to the other categories (v-test is therefore positive),
# and from least characteristic to most when the category is underrepresented in the class (v-test is negative),

# The individuals who are "Greeen"  are most significantly characterised by the fact that they are implicated a lot
# (v-test is positive and highest absolute value)
# the second is Age = [41;60]

# The p-value of the v-test is provided along with the associated v-test
# v-test null hypothesis:
# "the values of X for the individuals who chose the category q are selected at random from all of the possible values of X."

