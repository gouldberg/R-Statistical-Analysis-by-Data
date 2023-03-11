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
# Plot correlation functions as contours
# ------------------------------------------------------------------------------

gaitvarbifd <- var.fd(gaitfd)

str(gaitvarbifd)


gaitvararray <- eval.bifd(gaittime, gaittime, gaitvarbifd)



# ----------
gaitCorArray <- cor.fd(gaittime, gaitfd)

quantile(gaitCorArray)



# ----------
par(mfrow = c(2,2))

contour(gaittime, gaittime, gaitCorArray[,,1,2], cex=1.2)
title("Knee - Hip")


# ----------
contour(gaittime, gaittime, gaitCorArray[,,1,1], cex=1.2)
title("Knee - Knee")

contour(gaittime, gaittime, gaitCorArray[,,1,3], cex=1.2)
title("Hip - Hip")

