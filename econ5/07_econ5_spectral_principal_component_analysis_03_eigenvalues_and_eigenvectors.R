setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")

str(econ5)

econ5



# ------------------------------------------------------------------------------
# Eigen value Decomposition of spectral matrix
# ------------------------------------------------------------------------------


# spectral matrix: 5 * 5 * 80
dim(gr.spec$fxx)



# ----------
n.freq <- length(gr.spec$freq)


lam <- matrix(0, n.freq, 5)


for(k in 1:n.freq) lam[k,] <- eigen(gr.spec$fxx[,,k], symmetric = TRUE, only.values = TRUE)$values




# ----------
graphics.off()

par(mfrow = c(2, 1), mar = c(4,2,2,1), mgp = c(1.6, 0.6, 0))

plot(gr.spec$freq, lam[,1], type = "l", ylab = "", xlab = "Frequency", main = "First Eigenvalue")

abline(v = 0.25, lty = 2)

plot(gr.spec$freq, lam[,2], type = "l", ylab = "", xlab = "Frequency", main = "Second Eigenvalue")

abline(v = 0.125, lty = 2)




# -->
# These eigenvalues suggest the first factor is identified by the freequency of one cycle every 4 years,
# whereas the second factor is identified by the frequency of one cycle every 8 years.



# ------------------------------------------------------------------------------
# Eigen vectors
# ------------------------------------------------------------------------------

# eigen vectors at frequency 10/160 and 5/160 corresponding to 1st and 2nd eigen values

e.vec1 <- eigen(gr.spec$fxx[,,10], symmetric = TRUE)$vectors[,1]

e.vec2 <- eigen(gr.spec$fxx[,,5], symmetric = TRUE)$vectors[,2]

round(Mod(e.vec1), 3)

round(Mod(e.vec2), 3)



# -->
# The modulus of the corresponding eigenvectors at the frequencies of interest,

# These values confirm Unemployment, GNP, Consumption, and Private Investment load on the first factor,
# and Government Investment loads on the second factor.  (0.94)




