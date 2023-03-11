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
# Plot variance and covariance as surfaces
# ------------------------------------------------------------------------------

gaitvarbifd <- var.fd(gaitfd)

str(gaitvarbifd)


gaitvararray <- eval.bifd(gaittime, gaittime, gaitvarbifd)



# ----------
par(mfrow=c(2,2))

persp(gaittime, gaittime, gaitvararray[,,1,1], cex=1.2)
title("Knee - Knee")

persp(gaittime, gaittime, gaitvararray[,,1,2], cex=1.2)
title("Knee - Hip")

persp(gaittime, gaittime, gaitvararray[,,1,3], cex=1.2)
title("Hip - Hip")



