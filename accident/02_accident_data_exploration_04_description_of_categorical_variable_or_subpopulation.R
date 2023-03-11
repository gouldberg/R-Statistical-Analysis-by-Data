setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


# frequency form in dataframe to table
( acci.tab <- xtabs(Freq ~ age + result + mode + gender, data = Accident) )



# ----------
# convert table to case form
acci_c <- expand.dft(acci.tab)

head(acci_c)



# ------------------------------------------------------------------------------
# Description of a categorical variable by a categorical variable
# ------------------------------------------------------------------------------

library(FactoMineR)


# num.var = 2:  "result"
acci.catd <- catdes(acci_c, num.var = 2)


acci.catd



# ----------
# X^2 test
acci.catd$test.chi


# -->
# The smaller the p-value associated with the X^2 test, the more questionable the independence hypothesis.
# and the more the categorical varaible characterises the variable "result".



# ------------------------------------------------------------------------------
# Close investigation
# ------------------------------------------------------------------------------

( tab <- xtabs(~ result + age, data = acci_c) )


mosaic(tab, gp = shading_Friendly2, gp_arg = list(interpolate = 1:4), labeling = labeling_residuals)




# ----------
# Description of the Category "Died" of the variable "result" by the categories of the categorical variables
acci.catd$category$Died

addmargins(tab)


