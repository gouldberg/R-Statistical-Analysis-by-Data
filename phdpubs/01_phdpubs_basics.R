setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
#  - Long (1990, 1987) gave data on the number of publications by 915 doctoral condidates in biochemistry in the last 3 years
#    of their PhD studies.
#    The data set also includes information on:
#      - gender, marital status, number of young children, prestige of the doctoral department, and number of publications by the student's mentor 
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
