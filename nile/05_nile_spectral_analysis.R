rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------

data(Nile)


dim(Nile)


str(Nile)



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
# ------------------------------------------------------------------------------

length(Nile)

nextn(length(Nile))




# ----------
par(mfrow=c(2,1))


# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
nile.per <- astsa::mvspec(Nile, log = "no")
abline(v = 0.365, lty = 2, col = "gray")

nile2.per <- astsa::mvspec(diff(Nile), log = "no")
abline(v = 0.365, lty = 2, col = "gray")


# -->
# 3 years (frequency = 0.365) some cycle is strong



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

( ker <- kernel("modified.daniell", c(3,3)) )

plot(ker)


# ----------
# Lh = 1 / sum(hk^2)  k = -6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6  --> 9.24, which is close to the value of L = 9
1 / sum(ker$coef[c(6,5,4,3,2,1,2,3,4,5,6)]^2)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.25:  tapering the upper and lower 25% of the data

graphics.off()
par(mfrow=c(1,1))

nile.smo <- astsa::mvspec(Nile, kernel = ker, taper = 0.25, log = "no")
abline(v = 0.365, lty = 2, col = "gray")
