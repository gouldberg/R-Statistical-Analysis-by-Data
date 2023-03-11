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
# Distribution for each variables
# ------------------------------------------------------------------------------

psych::describe(temperature)


Hmisc::describe(temperature)



# ------------------------------------------------------------------------------
# Correlation among variables
# ------------------------------------------------------------------------------

round(cor(temperature[,1:12]), digits = 3)


psych::pairs.panels(temperature[,1:12])



