setwd("//media//kswada//MyFiles//R//dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dbbmi
# ------------------------------------------------------------------------------
data("dbbmi", package = "gamlss.data")


str(dbbmi)

car::some(dbbmi)



# ------------------------------------------------------------------------------
# Plot of distribution of y for specific values of x
# ------------------------------------------------------------------------------

library(gamlss.util)

plotSimpleGamlss(bmi, age, m0, data = dbbmi1, x.var = seq(5, 20, 5), xlim = c(-3, 23), val = 5)

