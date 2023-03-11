setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
#  - The data provided by Dr. Eileen M. Wright.
#    The response variable is abdominal circumference (y) and the explanatory variable is gestational age in weeks (x).
#    The data comprises 610 observations.
# ------------------------------------------------------------------------------
data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

car::scatterplot(y = abdom$y, x = abdom$x)


