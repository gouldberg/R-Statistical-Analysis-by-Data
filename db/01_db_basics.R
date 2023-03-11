setwd("//media//kswada//MyFiles//R//db")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  db
#  - Data from the Fourth Dutch Growth Study, which was a cross-sectional study that measured growth and development
#    of the Dutch population between the ages 0 and 21 years.
#    The data were kindly provided by Professor Stef van Buuren.
#  - The study measured, amongst other variables, height, weight, head circumference and age.
#  - Cases with missing values have been removed. There are 7,040 observations
#  - Variables:
#       - age (years) and head circumference in cm
# ------------------------------------------------------------------------------
data("db", package = "gamlss.data")


str(db)

car::some(db)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


lattice::xyplot(head ~ age, data = db, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))
