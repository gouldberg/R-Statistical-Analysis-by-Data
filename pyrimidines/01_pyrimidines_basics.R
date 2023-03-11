setwd("//media//kswada//MyFiles//R//pyrimidines")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pyrimidines
#   - Structural information on 74 2,4-diamino- 5-(substituted benzyl) pyrimidines used as inhibitors of DHFR in E. coli.
#     There are 3 positions where chemical activity occurs and 9 attributes per positionleading to 27 total predictors.
#     One predictor had no variability and was removed from the data set.26 chemical properties of 74 compounds and an activity level
#   - This concerns the modeling of the quantitative structure-activity relationships (QSAR) of the inhibition of dihydrofolate reductase (DHFR)
#     by pyrimidines.
#     We want to relate the psysicochemical and/or structural properties as exhibited by the 26 predictors in this data with an activity level.
# ------------------------------------------------------------------------------

data("pyrimidines", package = "faraway")

str(pyrimidines)

car::some(pyrimidines)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

psych::describe(pyrimidines)



# ----------
psych::pairs.panels(pyrimidines[,c("activity", "p1.polar", "p1.size", "p1.flex")])



# ----------
# by default, adding a fitted regression line (OLS line), and adding boxplots for x and y
car::scatterplot(activity ~ p1.polar, data = pyrimidines)
car::scatterplot(activity ~ p1.size, data = pyrimidines)
car::scatterplot(activity ~ p1.flex, data = pyrimidines)



