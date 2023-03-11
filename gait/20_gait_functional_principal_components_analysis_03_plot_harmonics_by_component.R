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
# Plot harmonics using cycle plots
# ------------------------------------------------------------------------------

graphics.off()

# 1st, 2nd
# 3rd, 4th components

op <- par(mfrow=c(2,2), mar = c(2,2,2,2))
plot.pca.fd(gaitpca.fd, cycle=TRUE)
par(op)


op <- par(mfrow=c(2,2), mar = c(2,2,2,2))
plot.pca.fd(gaitpca2.fd, cycle=TRUE)
par(op)
