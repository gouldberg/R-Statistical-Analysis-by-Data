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
# Compare various smoothings
# ------------------------------------------------------------------------------

lambda <- 1e-3
lipfd3 <- smooth.basisPar(argvals = liptime, y = lip, lambda = lambda)$fd
names(lipfd3$fdnames) <- c("time(seconds)", "replications", "mm")


lambda <- 1e-12
lipfd3.12 <- smooth.basisPar(liptime, lip, lambda = lambda)$fd
names(lipfd3.12$fdnames) <- c("time(seconds)", "replications", "mm")


lambda <- 1e-12
lipfd5 <- smooth.basisPar(liptime, lip, 6, Lfdobj=int2Lfd(2), lambda = lambda)$fd
names(lipfd5$fdnames) <- c("time(seconds)", "replications", "mm")


lambda <- 1e-12
lipfd5p <- smooth.basisPar(liptime, lip, 6, Lfdobj=int2Lfd(4), lambda = lambda)$fd
names(lipfd5p$fdnames) <- c("time(seconds)", "replications", "mm")



# ----------
graphics.off()
op <- par(mfrow=c(4,3), mar=c(2,2,2,2), pty="m", ask=FALSE)

plot(lipfd3,        main="Lip Position", cex=1.2)
plot(lipfd3, Lfd=1, ylab="mm / sec", main="Lip Velocity", cex=1.2)
plot(lipfd3, Lfd=2, ylab="mm / sec / sec", main="Lip Acceleration", cex=1.2)

plot(lipfd3.12,        main="Lip Position", cex=1.2)
plot(lipfd3.12, Lfd=1, ylab="mm / sec", main="Lip Velocity", cex=1.2)
plot(lipfd3.12, Lfd=2, ylab="mm / sec / sec", main="Lip Acceleration", cex=1.2)

plot(lipfd5,        main="Lip Position", cex=1.2)
plot(lipfd5, Lfd=1, ylab="mm / sec", main="Lip Velocity", cex=1.2)
plot(lipfd5, Lfd=2, ylab="mm / sec / sec", main="Lip Acceleration", cex=1.2)

plot(lipfd5p,        main="Lip Position", cex=1.2)
plot(lipfd5p, Lfd=1, ylab="mm / sec", main="Lip Velocity", cex=1.2)
plot(lipfd5p, Lfd=2, ylab="mm / sec / sec", main="Lip Acceleration", cex=1.2)


# This is quite different
plot(lipfd5, Lfd=4, ylab="mm / sec / sec", main="Lip Accel. of Accel.", cex=1.2)
plot(lipfd5p, Lfd=4, ylab="mm / sec / sec", main="Lip Accel. of Accel.", cex=1.2)

