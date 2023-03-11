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
# Compute residual variance-covariance matrix estmate
# ------------------------------------------------------------------------------

kneehatfd = gaitRegress$yhatfd$fd

kneehatmat = eval.fd(gaittime, kneehatfd)


resmat = gait[,,'Knee Angle'] - kneehatmat


( SigmaE = cov(t(resmat)) )


