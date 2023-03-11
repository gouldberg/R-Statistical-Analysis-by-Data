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
# Smoothed curves and its velocity and acceleration
# Ordinary cubic splines with smoothing parameters
# ------------------------------------------------------------------------------

# lipfd3 <- smooth.basisPar(argvals = liptime, y = lip)$fd
lambda <- 1e-3

lipfd3 <- smooth.basisPar(argvals = liptime, y = lip, lambda = lambda)$fd

names(lipfd3$fdnames) <- c("time(seconds)", "replications", "mm")


op <- par(mfrow=c(2,2), mar=c(5,5,4,2), pty="m", ask=FALSE)
plot(lipfd3,        main="Lip Position", cex=1.2)
plot(lipfd3, Lfd=1, ylab="mm / sec", main="Lip Velocity", cex=1.2)
plot(lipfd3, Lfd=2, ylab="mm / sec / sec", main="Lip Acceleration", cex=1.2)



# -->
# lines too straight, especially position and velocity due to too much smoothing



# ----------
# light smoothing
lambda <- 1e-12

lipfd3.12 <- smooth.basisPar(liptime, lip, lambda = lambda)$fd



names(lipfd3.12$fdnames) <- c("time(seconds)", "replications", "mm")

op <- par(mfrow=c(2,2), mar=c(5,5,4,2), pty="m", ask=FALSE)
plot(lipfd3.12,        main="Lip Position", cex=1.2)
plot(lipfd3.12, Lfd=1, ylab="mm/sec", main="Lip Velocity", cex=1.2)
plot(lipfd3.12, Lfd=2, ylab="mm/sec/sec", main="Lip Acceleration", cex=1.2)



# -->
# Acceleration not smooth at all ...
# We used cubic splines for location, so the velocity was parabolic splines and acceleration = linear splines (connected straight line segments)
# We need to use quintic splienes (degree 5 so order 6)


