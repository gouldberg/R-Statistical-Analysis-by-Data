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
# Fit the Posisson distribution to Death by horse kick data
# ------------------------------------------------------------------------------
tab <- as.data.frame(data, stringsAsFactors = FALSE)

colnames(tab) <- c("nDeaths", "Freq")

str(tab)



# ----------
# The mean number of deaths per corps-year = 1.61
( lambda <- weighted.mean(as.numeric(tab$nDeaths), w = tab$Freq) )



# ----------
# Calculate the probabilithies (phat) of k = 0:4 deaths, and hence the expected (exp) frequencies in a Poisson distribution
phat <- dpois(0:4, lambda = lambda)
exp <- sum(tab$Freq) * phat
chisq <- (tab$Freq - exp)^2 / exp


( GOF <- data.frame(tab, phat, exp, chisq) )



# ----------
# Chisq = 0.70537, shows an extremely good fit of these data to the Poisson distribution,
# perhaps exceptionally so.
# An exceptionally good fit occurs when the p-value for the test Chisq statistic is so high, as to suggest something unreasonable
# under random sampling might have occurred.
sum(chisq)

pchisq(sum(chisq), df = nrow(tab) - 2, lower.tail  = FALSE)

