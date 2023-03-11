setwd("//media//kswada//MyFiles//R//income")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  income
#   - married 734 males data
#       - lincome:  log of yearly income
#       - yeduc:  years of education
#       - payeduc:  father's years of education
# ------------------------------------------------------------------------------
income8 <- read.table("//media//kswada//MyFiles//references//計量経済学の第一歩//data//csv//8_income.csv", header = TRUE, sep = ",")

str(income8)



# ----------
library(psych)
pairs.panels(income8)



# ----------
hist(income8$lincom)

