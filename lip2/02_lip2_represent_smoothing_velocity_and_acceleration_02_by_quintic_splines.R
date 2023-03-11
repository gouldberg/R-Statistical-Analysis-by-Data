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
# Quicit basis (degree = 5 so order = 6)
# ------------------------------------------------------------------------------

lambda <- 1e-12

# lipbasis <- create.bspline.basis(range(liptime), 31, 6) 
# lipfd5 <- smooth.basisPar(liptime, lip, lipbasis, lambda = lambda)$fd
lipfd5 <- smooth.basisPar(liptime, lip, 6, int2Lfd(2), lambda = lambda)$fd


names(lipfd5$fdnames) <- c("time(seconds)", "replications", "mm")

op <- par(mfrow=c(2,2), mar=c(5,5,4,2), pty="m", ask=FALSE)
plot(lipfd5,        main="Lip Position", cex=1.2)
plot(lipfd5, Lfd=1, ylab="mm / sec", main="Lip Velocity", cex=1.2)
plot(lipfd5, Lfd=2, ylab="mm / sec / sec", main="Lip Acceleration", cex=1.2)


# -->
# Acceleration poorly smoothed
# The default smoothing operator = int2Lfd(2) = for location

# We need to use int2Lfd(4) to smooth acceleration of acceleration


