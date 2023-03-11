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
# Distribution for each variables
# ------------------------------------------------------------------------------

psych::describe(decathlon)


Hmisc::describe(decathlon)



# ------------------------------------------------------------------------------
# Correlation among variables
# ------------------------------------------------------------------------------

round(cor(decathlon[,1:10]), digits = 3)


psych::pairs.panels(decathlon[,1:10])



