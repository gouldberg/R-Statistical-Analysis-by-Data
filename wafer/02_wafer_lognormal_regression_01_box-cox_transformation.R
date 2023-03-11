setwd("//media//kswada//MyFiles//R//wafer")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wafer
# ------------------------------------------------------------------------------

data(wafer, package="faraway")

str(wafer)

car::some(wafer)



# ------------------------------------------------------------------------------
# check box-cox trans 
# ------------------------------------------------------------------------------

library(caret)

bc <- BoxCoxTrans(wafer$resist)

bc



# ----------
par(mfrow = c(2,1), mar = c(2,2,2,2))

hist(predict(bc, wafer$resist), breaks = seq(1.38, 1.41, 0.002))

hist(wafer$resist, breaks = seq(150,350,25))



# ----------
plot(density(wafer$resist))

plot(density(predict(bc, wafer$resist)))



# -->
# lambda = -0.7
# Box-Cox trans suggests transformation: { y^(-0.7) - 1 } / -0.7

