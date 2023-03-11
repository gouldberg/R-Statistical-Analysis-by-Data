setwd("//media//kswada//MyFiles//R//mice")


packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  mice
# ------------------------------------------------------------------------------

data(mice, package = "shapes")


dim(mice$x)



# ----------
# The coordinates in mice$x are also available individually by group
data(qcet2.dat, package = "shapes")
data(qlet2.dat, package = "shapes")
data(qset2.dat, package = "shapes")



# ------------------------------------------------------------------------------
# Plot landmark configurations by group
# ------------------------------------------------------------------------------

joins <- c(1,6,2:5,1)


par(mfrow=c(1,3))

plotshapes(mice$x[,,mice$group == "c"], joinline = joins)
title("Control")

plotshapes(mice$x[,,mice$group == "l"], joinline = joins)
title("Large")

plotshapes(mice$x[,,mice$group == "s"], joinline = joins)
title("Small")



# ------------------------------------------------------------------------------
# Plot outline by group
# ------------------------------------------------------------------------------

joins <- c(1:60, 1)


par(mfrow=c(1,3))

plotshapes(mice$outlines[,,mice$group == "c"], joinline = joins, col = 2)
title("Control")

plotshapes(mice$outlines[,,mice$group == "l"], joinline = joins, col = 2)
title("Large")

plotshapes(mice$outlines[,,mice$group == "s"], joinline = joins, col = 2)
title("Small")
