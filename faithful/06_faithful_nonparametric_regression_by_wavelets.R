setwd("//media//kswada//MyFiles//R//faithful")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  faithful
# ------------------------------------------------------------------------------

data(faithful, package="faraway")

str(faithful)


# ----------
# for comparison
# exa:  true function is f(x) = sin^3(e * pi * x^3)
# exb:  constant zero

data(exa);  data(exb)

str(exa)

str(exb)



# ------------------------------------------------------------------------------
# plot data
# ------------------------------------------------------------------------------

par(mfrow=c(1,3))

plot(y ~ x, exa, main = "Example A", pch = ".")
lines(m ~ x, exa)

plot(y ~ x, exb, main = "Example B", pch = ".")
lines(m ~ x, exb)

plot(waiting ~ eruptions, faithful, main = "Old Faithful", pch = ".")



# ------------------------------------------------------------------------------
# Non-parametric regresssion by Wavelets:  Haar basis
#  - We approximate the curve by a family of basis functions phi-i(x), so that f(x) = sum(ci * phi-i(x))
#    The estimation of ci is particularly easy if the basis functions phi-i(x) are orthogonal.
#    Examples of orthogonal bases are orthogonal polynomials and the Fourier basis, but the disadvantage of both these families is
#    that ths basis functions are not compactly supported so that the fit of each basis function dependes on the whole data.
#    This means that these fits lack the desirable local fit properties.
#  - Cubic B-splies are compactly supported, but they are not orthogonal.
#
#  - Wavelets have the advantage that they are compactly supported and can be defined so as to possess the orthogonality property.
#    They also possess the multiresolution property which allows them to fit the grosser features of the curve while focusing on the finer detail where necessary.
# ------------------------------------------------------------------------------

library(wavethresh)


# wavelet decomposition
# filter.number specifies the complexity of the family.
# Haar basis is the simplest but not the default choice.
# default filter number is 10
wds <- wd(exa$y, filter.number = 1, family = "DaubExPhase")

wds



# ----------
# show mother wavelet and wavelet coefficients
par(mfrow=c(1,2))
draw(wds, main = "")

plot(wds, main = "")



# ----------
# We retain only level-three and higher coefficients, these are only 2^3 = 8 of these.
# The thresholding here applies to level four and higher only by default.
# Any coefficients less than 9999 in absolute value is set to zero, that is, all of them in this case.
wtd <- threshold(wds, policy = "manual", value = 9999)
wtd2 <- threshold(wds)


# wr inverts the wavelet tranform.
fd <- wr(wtd)
fd2 <- wr(wtd2)



# ----------
# All level-four and above coefficients are zeroed.
# We see that the fit consists of eight constant fits.
par(mfrow=c(1,2))
plot(y ~ x, exa, col = gray(0.75))
lines(m ~ x, exa)
lines(fd ~ x, exa, lty = 5, lwd = 2)


# the coefficients are thresholded using the default method
# true function is shown as a solid line and the estimate as a dashed line.
# now the segments are of varying lenghts.
plot(y ~ x, exa, col = gray(0.75))
lines(m ~ x, exa)
lines(fd2 ~ x, exa, lty = 5, lwd = 2)



# -->
# We could view the thresholded coefficients as a compressed version of the original data (or signal)
# Some information has been lost in the compression, but thethresholding algorithm ensures that we tend to keep the detail we need, while throwing away
# noisier elements.



# ----------
# default version
wds <- wd(exa$y)
wds
draw(wds, main = "")
plot(wds, main = "")

wtd <- threshold(wds)
fd <- wr(wtd)
plot(y ~ x, exa, col = "gray")
lines(m ~ x, exa)
lines(fd ~ x, exa, lty = 5, lwd = 2)



# ----------
wds <- wd(faithful$eruptions[1:2^8])
wds
draw(wds, main = "")
plot(wds, main = "")

wtd <- threshold(wds)
fd <- wr(wtd)

fd

# -->
# fd is very small compared to waiting.....why ? ...
faithful$waiting



# ------------------------------------------------------------------------------
# Non-parametric regresssion by Wavelets:  Daubechies orthonormal compactly supported wavelet N=2 from the extremal phase family
#   - We would like to use continuous basis functions while retaining the orthogonality and multiresolution properties.
# ------------------------------------------------------------------------------

wds <- ed(exa$y, filter.number = 2, bc = "interval")


par(mfrow=c(1,2))
draw(filter.number = 2, family = "DaubExPhase")
plot(wds)



# ----------
# Now we try the default thresholding and reconstruct the fit
wtd <- threshold(wds)
fd <- wr(wtd)


par(mfrow=c(1,1))
plot(y ~ x, exa, col = gray(0.75))
lines(m ~ x, exa)
lines(fd ~ x, exa, lty = 2)



# ------------------------------------------------------------------------------
# Non-parametric regresssion by Wavelets:  irregularly spaced data
#   - The standard wavelet fitting function is designed only for evenly spaced x with a numbr of obeservations in some power of two.
# ------------------------------------------------------------------------------
x <- with(faithful, (eruptions - min(eruptions)) / (max(eruptions) - min(eruptions)))


# makegrid takes a set of univariate (x,y) data with x arbitrary in (0,1) and linearly interpolates (x,y) to an equally spaced dyadic grid.
# the method works by interpolating an evenly spaced grid on (0,1) using the next highest power of two beyond the size of the data in this case 512.
gridof <- makegrid(x, faithful$waiting)


# this funciton performs the irregular wavelet transform as described in the paper by Kovac and Silverman.
wdof <- irregwd(gridof, bc="symmetric")

wtof <- threshold(wdof)

wrof <- wr(wtof)


plot(waiting ~ eruptions, faithful, col=grey(0.75))
with(faithful, lines(seq(min(eruptions), max(eruptions), len=512), wrof))


# -->
# This demonstrates one of the main advantages of the wavelet method in handling and revealing discontinuities.


