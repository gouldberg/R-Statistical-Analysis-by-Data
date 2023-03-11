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
# Compute spectral matrices
# ------------------------------------------------------------------------------

eq.P <- as.ts(eqexp[P,1:8])

eq.S <- as.ts(eqexp[S,1:8])


# means of P wave and S wave of earthquakes
eq.m <- cbind(rowMeans(eq.P), rowMeans(eq.S))

ex.P <- as.ts(eqexp[P, 9:16])

ex.S <- as.ts(eqexp[S, 9:16])


# means of P wave and S wave of explosions
( ex.m <- cbind(rowMeans(ex.P), rowMeans(ex.S)) )



# ----------
# Compute the Discrete Fourier Transform (DFT) 
n <- 1024

# Difference between earthquakes and explosions
m.diff <- mvfft(eq.m - ex.m) / sqrt(n)

# Difference between each P wave and means of P waves of earthquakes
eq.Pf <- mvfft(eq.P - eq.m[,1]) / sqrt(n)

# Difference between each S wave and means of S waves of earthquakes
eq.Sf <- mvfft(eq.S - eq.m[,2]) / sqrt(n)

# Difference between each P wave and means of P waves of explosions
ex.Pf <- mvfft(ex.P - ex.m[,1]) / sqrt(n)

# Difference between each S wave and means of S waves of explosions
ex.Sf <- mvfft(ex.S - ex.m[,2]) / sqrt(n)




# ----------
N <- 8

fv11 <- rowSums(eq.Pf * Conj(eq.Pf)) + rowSums(ex.Pf * Conj(ex.Pf)) / (2 * (N - 1))

fv12 <- rowSums(eq.Pf * Conj(eq.Sf)) + rowSums(ex.Pf * Conj(ex.Sf)) / (2 * (N - 1))

fv22 <- rowSums(eq.Sf * Conj(eq.Sf)) + rowSums(ex.Sf * Conj(ex.Sf)) / (2 * (N - 1))

fv21 <- Conj(fv12)

