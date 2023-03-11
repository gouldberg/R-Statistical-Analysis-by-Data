setwd("//media//kswada//MyFiles//R//meuse")

packages <- c("dplyr", "maptools", "spdep", "RColorBrewer", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


pal = function(n = 9) brewer.pal(n, "Reds")



# ------------------------------------------------------------------------------
# data:  meuse
# ------------------------------------------------------------------------------
library(sp)

data(meuse)



# ----------
# This is data frame
str(meuse)

car::some(meuse)



