setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
#   - Various bivariate earthquakes (EQ) and explosions (EX) recorded at 40 pts/s including an even NZ (Novaya Zemlya)
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)



# ----------
P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name



# ------------------------------------------------------------------------------
# Test of equality means
#
#   - lambda =  |SPE| / |SPE + SPR|
#       SPE:  within cross-power series
#       SPR:  between cross-power
#
#   - The test statistic = -2 * (sum(Ni) - I - p - 1) * log(lambda)
#     approximately follows the chi-squared with 2(I - 1) * p degrees of freedom when the group means are equal.
#       Ni:  indivisuals in group i    I:  number of groups   p: p-variate time series
#
#   - The case of I = 2 groups reduces to Hotelling's T2 statistics
# ------------------------------------------------------------------------------

T2 <- rep(NA, 512)

for(k in 1:512){
  fvk <- matrix(c(fv11[k], fv21[k], fv12[k], fv22[k]), 2, 2)
  dk <- as.matrix(m.diff[k,])
  T2[k] <- Re((N/2) * Conj(t(dk)) %*% solve(fvk, dk))
}



# ----------
# The test statistic (eF)
p.dim <- 2

eF <- T2 * (2 * p.dim * (N - 1)) / (2 * N - p.dim - 1)




# ----------
# data are recorded 40pts/s
pts <- 40

freq <- pts * (0:511) / n


# Test statistic and threshold
graphics.off()
par(mfrow=c(1,1), mar = c(3,3,2,1), mgp = c(1.6, 0.6, 0), cex.main = 1.1)

plot(freq, eF, type = "l", xlab = "Frequency(Hz)", ylab = "F Statistic", main = "Equal Means")
abline(h = qf(.999, 2 * p.dim, 2 * (2 * N - p.dim - 1)))


qf(.999, 2 * p.dim, 2 * (2 * N - p.dim - 1))


# -->
# The test statistic (eF) remains well below its critical value at all frequencies,
# implying that the means of the two classes of series are not significantly different.
