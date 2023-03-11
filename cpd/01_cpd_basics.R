setwd("//media//kswada//MyFiles//R//cpd")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cpd
#   - In Whitmore (1986), some sales data on a range of products is presented for the projected, xi, and actual, yi,
#     sales for i = 1, ..., 20.
#     Projected and actual sales of 20 consumer products. Data have been disguised from origian form
# ------------------------------------------------------------------------------

data(cpd, package="faraway")

str(cpd)


car::some(cpd)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

psych::pairs.panels(cpd)


psych::describe(cpd)


car::scatterplot(projected ~ actual, data = cpd)