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
# Quicit basis (degree = 5 so order = 6) and Penalize the 4th derivative, not the second
# ------------------------------------------------------------------------------

lambda <- 1e-12

lipfd5p <- smooth.basisPar(liptime, lip, 6, Lfdobj=int2Lfd(4), lambda = lambda)$fd

names(lipfd5p$fdnames) <- c("time(seconds)", "replications", "mm")


op <- par(mfrow=c(2,2), mar=c(5,5,4,2), pty="m", ask=FALSE)
plot(lipfd5p,        main="Lip Position", cex=1.2)
plot(lipfd5p, Lfd=1, ylab="mm / sec", main="Lip Velocity", cex=1.2)
plot(lipfd5p, Lfd=2, ylab="mm / sec / sec", main="Lip Acceleration", cex=1.2)

