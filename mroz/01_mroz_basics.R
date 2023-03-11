setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mroz
#   - data on wages and their determinants for 753 observartions
# ------------------------------------------------------------------------------

data(mroz, package = "POE5Rdata")


dim(mroz)

str(mroz)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
