setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)



# ------------------------------------------------------------------------------
# Plot variance and covariance as contours
# ------------------------------------------------------------------------------

gaitvarbifd <- var.fd(gaitfd)

str(gaitvarbifd)


gaitvararray <- eval.bifd(gaittime, gaittime, gaitvarbifd)



# ----------
# plot variance and covariance functions as contours

filled.contour(gaittime, gaittime, gaitvararray[,,1,2], cex=1.2)
title("Knee - Hip")



# ----------
filled.contour(gaittime, gaittime, gaitvararray[,,1,1], cex=1.2)
title("Knee - Knee")


filled.contour(gaittime, gaittime, gaitvararray[,,1,3], cex=1.2)
title("Hip - Hip")


