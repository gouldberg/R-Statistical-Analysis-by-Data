setwd("//media//kswada//MyFiles//R//arrests")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra", "carData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arrests
# ------------------------------------------------------------------------------
data("Arrests", package = "carData")

dim(Arrests)

str(Arrests)

car::some(Arrests)

data <- Arrests

# To allow for possible nonlinear effects of year, this variable was treated as a factor rather than as a (linear) numeric variable
data$year <- as.factor(data$year)



# ------------------------------------------------------------------------------
# Generalized pairs plot
# ------------------------------------------------------------------------------

library(gpairs)
library(vcd)


gpairs(Arrests, diag.pars = list(fontsize = 20, hist.color = "gray"), mosaic.pars = list(gp = shading_Friendly), outer.rot = c(45, 45))
