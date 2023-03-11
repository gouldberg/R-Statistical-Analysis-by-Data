setwd("//media//kswada//MyFiles//R//lip2")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lip2
#   - The data of the movement of lips during speech production
# ------------------------------------------------------------------------------

data(lip, package = "fda")

dim(lip)


head(lip)


# -----------
liptime



# ------------------------------------------------------------------------------
# Individual plot
# ------------------------------------------------------------------------------

par(mfrow=c(2,2))
plotfit.fd(lip, liptime, lipfd5p)



# Residual plot
plotfit.fd(lip, liptime, lipfd5p, residual=TRUE, type='b', sortwrd=TRUE)
