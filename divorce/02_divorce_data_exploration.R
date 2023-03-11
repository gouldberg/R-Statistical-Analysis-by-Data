# setwd("//media//kswada//MyFiles//R//divorce//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//divorce//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  divorce
# ------------------------------------------------------------------------------

source("divorce_data.R")

length(dat)

data.ls <- list(J = length(dat), tt = dat)

table(dat)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

dat


summary(dat)


psych::describe(dat)