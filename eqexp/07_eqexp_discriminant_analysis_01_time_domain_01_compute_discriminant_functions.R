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
# Compute log of the maximum peak-to-peak amplitudes
# ------------------------------------------------------------------------------

# logarithms (base 10) of the maximum peak-to-peak amplitudes can be considered as two-dimensional feature vectors
mag.P <- log10(apply(eqexp[P,], 2, max) - apply(eqexp[P,], 2, min))

mag.S <- log10(apply(eqexp[S,], 2, max) - apply(eqexp[S,], 2, min))

eq.P <- mag.P[1:8]

eq.S <- mag.S[1:8]

ex.P <- mag.P[9:16]

ex.S <- mag.S[9:16]

NZ.P <- mag.P[17]

NZ.S <- mag.S[17]



# ------------------------------------------------------------------------------
# Compute linear discriminant function
# ------------------------------------------------------------------------------

( cov.eq <- var(cbind(eq.P, eq.S)) )

( cov.ex <- var(cbind(ex.P, ex.S)) )



# covariance matrices are assumed to be equal

( cov.pooled <- (cov.ex + cov.eq)/2 )

means.eq <- colMeans(cbind(eq.P, eq.S))

means.ex <- colMeans(cbind(ex.P, ex.S))


slopes.eq <- solve(cov.pooled, means.eq)

inter.eq <- -sum(slopes.eq * means.eq)/2

slopes.ex <- solve(cov.pooled, means.ex)

inter.ex <- -sum(slopes.ex * means.ex)/2



# ----------
d.slopes <- slopes.eq - slopes.ex

d.inter <- inter.eq - inter.ex



# ----------
# print functions
cat(d.slopes[1], "mag.P + ", d.slopes[2], "mag.S +", d.inter, "\n")

