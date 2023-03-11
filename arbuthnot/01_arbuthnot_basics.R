setwd("//media//kswada//MyFiles//R//arbuthnot")

packages <- c("dplyr", "vcd", "MASS", "HistData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arbuthnot data
#  - Sex ratios, such as births of male to female children, have long been of interest in population studies and demography.
#    Indeed, in 1710, John Arbuthnot used data on the ratios of male to female christenings in London from 1629 - 1710 to carry out
#    the first known significance test.
#    The data for these 82 years showed that in every year there were more boys than girls.
#    He calculated that, under the assumption that male and female births were equally likely, the probability of 82 years of more males than females
#    was vanishingly small. He used this to argue that a nearly constant birth ratio > 1 (or Pr(Male) > 0.5) could be interpreted to show the
#    guiding hand of a divine being.
# ------------------------------------------------------------------------------

data("Arbuthnot", package = "HistData")


str(Arbuthnot)


dim(Arbuthnot)


car::some(Arbuthnot)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
