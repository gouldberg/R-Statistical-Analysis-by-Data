setwd("//media//kswada//MyFiles//R//temperature")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  temperature
# ------------------------------------------------------------------------------

temperature <- read.table("temperature.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(temperature)

dim(temperature)


temperature



# ------------------------------------------------------------------------------
# Description of a categorical variable by a categorical variable
# ------------------------------------------------------------------------------

library(FactoMineR)


# num.var = 17 here:  Area
temp.catd <- catdes(temperature[1:23,], num.var = 17)


temp.catd



# ----------
# X^2 test
temp.catd$test.chi




