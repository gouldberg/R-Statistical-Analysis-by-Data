setwd("//media//kswada//MyFiles//R//pinch")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pinch
# ------------------------------------------------------------------------------

pinch <- matrix(scan("pinch.txt", 0), 151, 20, byrow=TRUE)

# data("pinch", package = "fda")

head(pinch)



# ------------------------------------------------------------------------------
# Functional Principal Component Analysis
# ------------------------------------------------------------------------------

lambda      <- 1e-4
pcafdPar    <- fdPar(pinchbasis, 2, lambda)

pinchpca.fd <- pca.fd(pinchfd, nharm = 3, pcafdPar)



# ----------
graphics.off()
par(mfrow = c(2,2))
plot(pinchpca.fd)



# ------------------------------------------------------------------------------
# Varimax rotation
# ------------------------------------------------------------------------------

pinchpca.fd_v <- varmx.pca.fd(pinchpca.fd)


graphics.off()
par(mfrow = c(2,3))
plot(pinchpca.fd)
plot(pinchpca.fd_v)



# ------------------------------------------------------------------------------
# # of eigenvalue and eigenvalues
# ------------------------------------------------------------------------------

pincheigvals <- pinchpca.fd[[2]]

par(mfrow=c(1,1),pty="s")
plot(1:19, log10(pincheigvals[1:19]), type="b", xlab="Eigenvalue Number", ylab="Log 10 Eigenvalue")
abline(lsfit(4:19, log10(pincheigvals[4:19])), lty=2)

