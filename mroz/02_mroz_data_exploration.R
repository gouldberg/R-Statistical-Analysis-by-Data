setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mroz
# ------------------------------------------------------------------------------

data(mroz, package = "POE5Rdata")


dim(mroz)

str(mroz)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

Hmisc::describe(mroz)


psych::describe(mroz)



# ----------
# distribution of wage by lfp (lfp == 1:  women in the labour force)
lattice::histogram(~ wage | lfp, data = mroz)

