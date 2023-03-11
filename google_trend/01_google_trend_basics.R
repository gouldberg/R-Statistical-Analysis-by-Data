# setwd("//media//kswada//MyFiles//R//google_trend//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//google_trend//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  google trend
# ------------------------------------------------------------------------------

library(tidyverse)

s.dfData2 <- read_csv("dfData2.csv", locale = locale(encoding = "cp932"))


str(s.dfData2)


dim(s.dfData2)


car::some(s.dfData2)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

