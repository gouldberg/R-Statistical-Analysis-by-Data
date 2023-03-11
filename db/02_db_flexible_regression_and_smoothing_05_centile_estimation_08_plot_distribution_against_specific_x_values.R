setwd("//media//kswada//MyFiles//R//db")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  db
# ------------------------------------------------------------------------------
data("db", package = "gamlss.data")


str(db)

car::some(db)



# ------------------------------------------------------------------------------
# Plot of distribution of y for specific values of x
# ------------------------------------------------------------------------------

library(gamlss.util)

plotSimpleGamlss(head, age, m0_c, data = db, x.var = seq(5, 20, 5), xlim = c(-3, 23))

plotSimpleGamlss(head, age, m0_c, data = db, x.var = seq(1, 22, 7), xlim = c(-8, 23))
