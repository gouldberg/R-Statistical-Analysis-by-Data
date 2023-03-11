setwd("//media//kswada//MyFiles//R//horse_kicks")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death by horse kick
# ------------------------------------------------------------------------------
data("HorseKicks", package = "vcd")

data <- HorseKicks

data
sum(data)



# ------------------------------------------------------------------------------
# Diagnosing discrete distributions:  Ord plots  by Ord_plot
#   - The summary goodness-of-fit statistics can easily be influenced by one or two disparate cells, or a additional (ignored or unknown) factors.
#     One simple alternative is a plot suggested by Ord (1967), which may be used to diagnose the form of the discrete distribution.
#   - Ord showed that a linear relationship of the form:
#        k * p(k) / (P * (k - 1))  =  k * n(k) / n(k-1) = a + b * k
#     holds for each of the Poisson, binomial, negative binomial, and logarithmic series distributions,
#     and these distributions are distinguished by the signs of the intercept, a, and slope, b.
#   - Diagnostic slope and intercept for 4 discrete distributions
#       - Poisson(lambda):  lambda = a,          Intercept(a) > 0,  Slope(b) = 0
#       - Binomial(n, p):  p = b / (b - 1),      Intercept(a) > 0,  Slope(b) < 0
#       - Negative binomial(n, p):  p = 1 - b,   Intercept(a) > 0,  Slope(b) > 0
#       - Log. series(theta):  theta = b,        Intercept(a) < 0,  Slope(b) > 0
#       - Log. series(theta):  theta = -a
# ------------------------------------------------------------------------------


vcd::Ord_plot(data, main = "Death by horse kicks", gp = gpar(cex = 1), pch = 16)



