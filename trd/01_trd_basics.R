setwd("//media//kswada//MyFiles//R//trd")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  trd (Tokyo rainfall data)
#  - The data are taken from Kitagawa and contain observations from the years 1983 and 1984.
#    They record whether there is more than 1 mm rainfall in Tokyo.
#    The data consists of 366 observations of the variable trd, which takes values 0, 1, 2 on the number of times there was rain on the specific day of the year
#    (during the two year period)
#    Observation number 60 corresponds to 29 February, and therefore only one day is observed during the two years.
#  - Variables
#       - trd:  values 0, 1, or 2
# ------------------------------------------------------------------------------

data("trd", package = "gamlss.data")


str(trd)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


