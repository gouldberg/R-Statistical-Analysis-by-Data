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
# Test equality of two univariate spectra on P components using N1 = N2 = 8
# ------------------------------------------------------------------------------

N


# ----------
# We need to smooth specra over L frequencies
m <- 10

L <- 2 * m + 1


kd <- kernel("daniell", m)



# ----------
u <- Re(rowSums(eq.Pf * Conj(eq.Pf)) / (N - 1))
feq.P <- kernapply(u, kd, circular = TRUE)


u <- Re(rowSums(ex.Pf * Conj(ex.Pf)) / (N - 1))
fex.P <- kernapply(u, kd, circular = TRUE)


# ----------
plot(freq, feq.P[1:512] / fex.P[1:512], type = "l", xlab = "Frequency (Hz)", ylab = "F Statistic", main = "Equal P-Spectra")


# We need to replace (N-1) to L * (N-1) here
( thre <- qf(0.999, 2 * L * (N - 1), 2 * L * (N - 1)) )
abline(h = thre)



# -->
# strong rejection of the euqal spectra hypothesis



# ------------------------------------------------------------------------------
# Test equality of two univariate spectra on S components using N1 = N2 = 8
# ------------------------------------------------------------------------------

N

kd <- kernel("daniell", m)



# ----------
u <- Re(rowSums(eq.Sf * Conj(eq.Sf)) / (N - 1))
feq.S <- kernapply(u, kd, circular = TRUE)


u <- Re(rowSums(ex.Sf * Conj(ex.Sf)) / (N - 1))
fex.S <- kernapply(u, kd, circular = TRUE)



# ----------
plot(freq, feq.S[1:512] / fex.S[1:512], type = "l", xlab = "Frequency (Hz)", ylab = "F Statistic", main = "Equal S-Spectra")

# We need to replace (N-1) to L * (N-1) here
( thre <- qf(0.999, 2 * L * (N - 1), 2 * L * (N - 1)) )
abline(h = thre)



# -->
# strong rejection of the euqal spectra hypothesis.
# the rejection seems stronger for the S components.
# We might tentatively identify that component as being dominant,

