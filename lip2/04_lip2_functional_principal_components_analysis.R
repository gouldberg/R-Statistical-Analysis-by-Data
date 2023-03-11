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

lipmarks



# ------------------------------------------------------------------------------
# Funtional principal component analysis
# ------------------------------------------------------------------------------

# lambda <-1e-6
lambda <- 1e-12
lipfd <- smooth.basisPar(liptime, lip, 6, Lfdobj=int2Lfd(4), lambda = lambda)$fd

pcafdPar  <- fdPar(lipfd, 2, lambda)


# ----------
lippca.fd <- pca.fd(lipfd, nharm=3, pcafdPar)



# ----------
graphics.off()
par(mfrow=c(2,2), pty="m")
plot.pca.fd(lippca.fd)



# ----------
lipeigvals <- lippca.fd[[2]]

plot(1:19, log10(lipeigvals[1:19]), type="b",
     xlab="Eigenvalue Number", ylab="", main="Log10 Eigenvalues")
