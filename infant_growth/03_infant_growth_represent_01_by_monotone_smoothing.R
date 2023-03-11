setwd("//media//kswada//MyFiles//R//infant_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  infant growth
# ------------------------------------------------------------------------------

data("infantGrowth", package = "fda")


dim(infantGrowth)


head(infantGrowth)




# ------------------------------------------------------------------------------
# Monotone Smoothing
# ------------------------------------------------------------------------------

day    = infantGrowth[, 'day']

tib    = infantGrowth[, 'tibiaLength']

n      = length(tib)



# ----------
# a basis for monotone smoothing
nbasis = 42

Wbasis   = create.bspline.basis(c(1, n), nbasis)



# ----------
# the fdPar object for smoothing
Wfd0     = fd(matrix(0, nbasis, 1), Wbasis)

WfdPar   = fdPar(Wfd0, 2, 1e-4)



# ----------
# monotone smoothing
result   = smooth.monotone(day, tib, WfdPar)

Wfd      = result$Wfd

( beta     = result$beta )



# ----------
dayfine  = seq(1, n, len=151)

tibhat   = beta[1] + beta[2] * eval.monfd(dayfine, Wfd)



# ------------------------------------------------------------------------------
# plot smoothed trajectory
# ------------------------------------------------------------------------------

par(mfrow=c(1,1), lwd = 2)

plot(day, tib, type = "p", cex=1.2, las=1, xlab="Day", ylab='', main="Tibia Length (mm)")

lines(dayfine, tibhat, lwd=2)

