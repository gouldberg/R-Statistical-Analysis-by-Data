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
# Plot functions for intercept and Hip Coefficients
# ------------------------------------------------------------------------------

kneehatfd   <- gaitRegress$yhatfd


betaestlist <- gaitRegress$betaestlist

alphafd   <- betaestlist[[1]]$fd

hipbetafd <- betaestlist[[2]]$fd




# ----------
# plot functions for intercept and Hip coefficients

op <- par(mfrow=c(2,1), ask=FALSE)

plot(alphafd,   ylab="Intercept")

plot(hipbetafd, ylab="Hip coefficient")

par(op)




# ------------------------------------------------------------------------------
# Intercept
# ------------------------------------------------------------------------------

gaitfine = seq(0, 20, len = 101)

( kneeIntercept = predict(betaestlist$const$fd, gaitfine) )



# ----------
par(mfrow = c(1,2))

plot(kneeIntercept, type = "o")

plot(alphafd,   ylab="Intercept")



# ------------------------------------------------------------------------------
# Hip coefficient
# ------------------------------------------------------------------------------

( hipCoef = predict(betaestlist$hip$fd, gaitfine) )



# ----------
par(mfrow = c(1,2))

plot(hipCoef, type = "o")

plot(hipbetafd, ylab="Hip coefficient")

