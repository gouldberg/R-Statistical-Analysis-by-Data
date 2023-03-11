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
# basics (re-presentation)
# ------------------------------------------------------------------------------

matplot(x = liptime, y = lip, type = 'l',
        xlab='Normalized Time', ylab='lip position (mm)')


# -->
# This presents the position of the lowe lip when saying the word "Bob" 20 times.
# As is clear from the data, there are distinct opening and shutting phases of the mouth surrouding a fairly linear trend that
# corresponds to the vocalization of the vowel.
# Muscle tissue behaves in many ways like a spring.
# This observations suggests that we consider fitting a second-order equation to these data, like:
# D^2 x(t) = - beta0(t) * x(t) - beta1(t) * Dx(t) + alpha * g(t)

# 1st term:  position-dependent forces like a spring for which the "stress" (force) is proportional to the "strain" (deformation)
# 2nd term:  proportional to the speed at which the system moves, and can be though of in terms of friction of viscosity,
# especially when beta1 is positive.
# g(t) represents external inputs into a system that modify its behavior.



# ------------------------------------------------------------------------------
# Stability Analysis for Principle Differential Analysis
# Principal Differential Analysis for Linear Dynamics
# ------------------------------------------------------------------------------

# The following code attempts to derive a second-order homogeneous differential equation like expression above for
# lipfd obtained from smoothing the lip data with no smoothing in the coefficients beta0(t) and beta1(t)

lipfd = smooth.basisPar(liptime, lip, 6, Lfdobj=int2Lfd(4), lambda=1e-12)$fd

lipbasis <- lipfd$basis

names(lipfd$fdnames) = c("time(seconds)", "replications", "mm")

lipfd0 <- fd(matrix(0, lipbasis$nbasis, 1), lipbasis)

lipfdPar <- fdPar(lipfd0, 2, 0)

bwtlist   = list(lipfdPar, lipfdPar)

xfdlist   = list(lipfd)



# ----------
pdaList   = pda.fd(xfdlist, bwtlist)


# pda.fd() provides for arguments awtlist and ufdlist,
# whose absence here indicates that the forcing function is zero.
pdaList$awtlist



# ------------------------------------------------------------------------------
# Plot estimated beta0(t) and beta1(t), discriminant function, and overalay beta coefficients on the bifurcation diagram
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(pdaList, whichdim=1, lwd = 2)


# Results of performing principal differential analysis on the lip data.
# This represnet the estimated beta0(t) and beta1(t) functional coefficeints.
par(mfrow=c(1,1))
plot(pdaList, whichdim=3, cex.axis=1.5, cex.lab=1.5, lwd=2)



# ----------
# Overlays the results of a univariate, second-order principal differential analysis on a bifurcation
# diagram to demonstrate stability.
par(mfrow=c(1,1))
pda.overlay(pdaList, lwd=2, cex.lab=1.5, cex.axis=1.5)


# -->
# dashed line shows the discriminant d = beta1(t) ^ 2 / 4 - beta0(t) = 0
# d > 0:  increasing oscillations
# beta1 > 0:  Exponential Decay
# beta1 < 0:  Exponential Growth



# ----------
# beta0 and beta1
bwtestlist= pdaList$bwtlist
bwtestlist[[1]]$fd$fdnames = list('time','rep','beta0')
bwtestlist[[2]]$fd$fdnames = list('time','rep','beta1')


# discriminant function
dfd        = 0.25 * bwtestlist[[2]]$fd^2 - bwtestlist[[1]]$fd
dfd$fdnames= list('time','rep','discriminant')



# ----------
# Also discriminant function: beta0, beta1, and discriminant function (d)
op = par(mfrow=c(3,1))
plot(bwtestlist[[1]]$fd, cex.lab=1.5, cex.axis=1.5, lwd=2, main="beta 0")
plot(bwtestlist[[2]]$fd, cex.lab=1.5, cex.axis=1.5, lty=2, lwd=2, main="beta 1")
plot(dfd, cex.lab=1.5, cex.axis=1.5, lwd=2, main="discriminant")
par(op)



# -->
# There is an initial explosive motion as the lips, previously sealed, are opened.
# This is followed by a period where the motion of the lips is largely oscillatory with a period of about 30 - 40 ms.
# This corresponds approximately to the spring constant of flaccid muscle tissue.

# During the "o" phase of the word, there is a period of dampled behavior when the lips are kept open in order to enunciate the vowel.


