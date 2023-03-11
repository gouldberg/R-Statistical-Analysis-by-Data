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
# Plot both the unregistered and registered versions of the curves
# ------------------------------------------------------------------------------

op <- par(mfrow=c(2,2))

plot(D2gaitfd,    ask = FALSE)

plot(D2gaitregfd, ask = FALSE)

par(op)
