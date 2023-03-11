setwd("//media//kswada//MyFiles//R//dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dbbmi
#  - Data from the Fourth Dutch Growth Study, which was a cross-sectional study that measured growth and development
#    of the Dutch population between the ages 0 and 21 years.
#    The data were kindly provided by Professor Stef van Buuren.
#  - The study measured, amongst other variables, height, weight, head circumference and age.
#  - Variables:
#       - age (years) and bmi (Bodi Maxx Index)
# ------------------------------------------------------------------------------

data("dbbmi", package = "gamlss.data")


str(dbbmi)

car::some(dbbmi)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


lattice::xyplot(bmi ~ age, data = dbbmi, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))

