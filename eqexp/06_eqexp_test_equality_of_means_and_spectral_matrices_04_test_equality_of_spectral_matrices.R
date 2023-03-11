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
# Test of equality of spectral matrices
#   - This test evolves from the likelihood ratio criterion, 
#     which compares the single group spectral matrices with the pooled spectral matrix
# ------------------------------------------------------------------------------

u <- rowSums(eq.Pf * Conj(eq.Sf)) / (N-1)

feq.PS <- kernapply(u, kd, circular = TRUE)

u <- rowSums(ex.Pf * Conj(ex.Sf)) / (N-1)

fex.PS <- kernapply(u, kd, circular = TRUE)

fv11 <- kernapply(fv11, kd, circular = TRUE)

fv22 <- kernapply(fv22, kd, circular = TRUE)

fv12 <- kernapply(fv12, kd, circular = TRUE)



# ----------
# A modification of the likelihood ratio test incorporates the degrees of freedom as;
Mi <- L * (N - 1)

M <- 2 * Mi



# TS: test statistics
TS <- rep(NA, 512)

for(k in 1:512){
  det.feq.k <- Re(feq.P[k] * feq.S[k] - feq.PS[k] * Conj(feq.PS[k]))
  det.fex.k <- Re(fex.P[k] * fex.S[k] - fex.PS[k] * Conj(fex.PS[k]))
  det.fv.k <- Re(fv11[k] * fv22[k] - fv12[k] * Conj(fv12[k]))
  log.n1 <- log(M) * (M * p.dim)
  log.d1 <- log(Mi) * (2 * Mi * p.dim)
  log.n2 <- log(Mi) * 2 + log(det.feq.k) * Mi + log(det.fex.k) * Mi
  log.d2 <- (log(M) + log(det.fv.k)) * M
  r <- 1 - ((p.dim + 1) * (p.dim - 1) / 6 * p.dim * (2-1)) * (2/Mi - 1/M)
  TS[k] <- -2 * r * (log.n1 + log.n2 - log.d1 - log.d2)
}

plot(freq, TS, type = "l", xlab = "Frequency (Hz)", ylab = "Chi-sq Statistic", main = "Equal Spectral Matrices")


( thresh <- qchisq(.999, p.dim ^ 2) )

abline(h = thresh)



# -->
# This version of testing equality of the spectral matrices shows a similar strong rejection of the equality of spectral matrices.

