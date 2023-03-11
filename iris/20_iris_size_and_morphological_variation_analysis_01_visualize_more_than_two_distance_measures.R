setwd("//media//kswada//MyFiles//R//iris")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  iris
# ------------------------------------------------------------------------------
data("iris")


str(iris)

car::some(iris)



# ------------------------------------------------------------------------------
# Show shape variation:
# Visualize of more than two distance measurements:  triangle-plots
#   - Triangle-plots is not a direct representation of raw data, since it arranges data so that the 3 variables present in one observation sum up to one.
#     However, it can be convenient if we consider the sum of variables to be a proxy for size.
# ------------------------------------------------------------------------------

library(ade4)


tp <- triangle.plot(iris[,c(1,2,4)], cpoint = 0, show.position =F)

points(tp, pch = c(1,2,8)[iris[,5]])

