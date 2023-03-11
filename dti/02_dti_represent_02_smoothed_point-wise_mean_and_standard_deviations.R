setwd("//media//kswada//MyFiles//R//dti")

packages <- c("dplyr", "fda", "refund")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Diffusion tensor imaging (DTI)
# ------------------------------------------------------------------------------
data("DTI", package = "refund")

str(DTI)



# ----------
( Y <- DTI$cca )


# exclude rows with missing values
Y <- Y[-c(126, 130, 131, 125, 319, 321),]



# ------------------------------------------------------------------------------
# Smoothed point-wise mean and standard deviations
# ------------------------------------------------------------------------------

argvals <- seq(0, 1, length = ncol(Y))


data_basis <- create.bspline.basis(c(0, 1), nbasis = 10)


Y.f <- Data2fd(argvals, t(Y), data_basis)

Y.f



# ----------
# mean
mean(Y.f)


# Standard deviation
std.fd(Y.f)



# ----------
par(mfrow=c(1,1))
plot(Y.f, lty = 1, col = "gray", xlab = "", ylab = "", ylim = c(0.1, 0.9))
lines(mean(Y.f), lwd = 2)
lines(mean(Y.f) + std.fd(Y.f), lwd = 2, lty = 2, col = "green")
lines(mean(Y.f) + 2 * std.fd(Y.f), lwd = 2, lty = 2, col = "yellow")
lines(mean(Y.f) + 3 * std.fd(Y.f), lwd = 2, lty = 2, col = "red")
lines(mean(Y.f) - std.fd(Y.f), lwd = 2, lty = 2, col = "green")
lines(mean(Y.f) - 2 * std.fd(Y.f), lwd = 2, lty = 2, col = "yellow")
lines(mean(Y.f) - 3 * std.fd(Y.f), lwd = 2, lty = 2, col = "red")

