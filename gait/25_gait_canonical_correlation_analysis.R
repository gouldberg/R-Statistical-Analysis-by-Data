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
# Canonical Components Analysis
# ------------------------------------------------------------------------------

( gaittime <- as.numeric(dimnames(gait)[[1]]) * 20 )


gaitrange <- c(0,20)

gaitbasis <- create.fourier.basis(gaitrange, nbasis=21)

harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=gaitrange)

gaitfd <- smooth.basisPar(gaittime, gait, gaitbasis, Lfdobj=harmaccelLfd, lambda=1e-2)$fd


str(gaitfd)
names(gaitfd$fdnames) <- c("Normalized time", "Child", "Angle")
gaitfd$fdnames[[3]] <- c("Hip", "Knee")



# ----------
hipfd  <- gaitfd[,1]

kneefd <- gaitfd[,2]

hipfdPar  <- fdPar(hipfd,  harmaccelLfd, 1e2)

kneefdPar <- fdPar(kneefd, harmaccelLfd, 1e2)



# ----------
ccafd    <- cca.fd(hipfd, kneefd, ncan=3, hipfdPar, kneefdPar)



# ------------------------------------------------------------------------------
# plot the canonical weight functions
# ------------------------------------------------------------------------------

op <- par(mfrow=c(2,1), mar=c(3,4,2,1), pty="m")
plot.cca.fd(ccafd, cex=1.2)
par(op)



# ----------
# display the canonical correlations
round(ccafd$ccacorr[1:6],3)

par(mfrow=c(1,1))
plot(1:6, ccafd$ccacorr[1:6], type="b")

