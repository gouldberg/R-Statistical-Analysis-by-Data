setwd("//media//kswada//MyFiles//R//federalist")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Trial:  Generate negative binomial distribution by Gamma-Poisson mixture
# ------------------------------------------------------------------------------


n0 = 65

n1 = 12




# ----------
# authors: Poissson distribution
lambda0 <- 10


( ham <- rpois(n = n0, lambda = lambda) )




# ----------
# Sole authors (Madison): Poisson distribution but the lambda is gamma distributed

mu <- lambda

k <- 10

theta <- mu / k


( mad_lambda <- round(rgamma(n = n1, shape = k, scale = theta), 0) )


( mad <- sapply(1:n1, function(i) rpois(n = 1, lambda = mad_lambda[i])) )




# ----------
ham_mad <- c(ham, mad)




# ----------
graphics.off()

par(mfrow=c(2,2))

hist(ham, breaks = seq(0, 10, by = 1), ylim = c(0, 80), main = "Authors's Occurrences")

hist(mad, breaks = seq(0, 10, by = 1), ylim = c(0, 80), main = "Madison's Occurrences")

hist(ham_mad, breaks = seq(0, 10, by = 1), ylim = c(0, 80), main = "Final Occurrences")

