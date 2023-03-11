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
# Smoothing by bspline
# ------------------------------------------------------------------------------

pinchtime  <- seq(0, 150, len = 151) / 600

pinchrange <- c(0, 0.25)



# ----------
nbasis <- 153

norder <-   4

pinchbasis <- create.bspline.basis(pinchrange, nbasis, norder)

lambda <- 1e-6

pinchfdPar <- fdPar(pinchbasis, 2, lambda)

pinchfd <- smooth.basis(pinchtime, pinch, pinchfdPar)$fd

names(pinchfd$fdnames) <- c("Arg. No.", "Replicates", "Force (N)")



# ----------
# plot all the curves
graphics.off()
par(mfrow=c(1,1), pty="m")

plot(pinchfd)
title("Pinch Force Curves")

