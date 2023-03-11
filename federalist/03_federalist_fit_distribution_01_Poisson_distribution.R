setwd("//media//kswada//MyFiles//R//federalist")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Federalist
# ------------------------------------------------------------------------------

data("Federalist", package = "vcd")

data <- Federalist


data

sum(data)




# ------------------------------------------------------------------------------
# Fit the posisson distribution to Federalist Papers data
#
#  - A naive hypothesis is that thesse occurrences might follow a Poisson distribution, that is,
#   as independent occurrences with constant probability across the 262 blocks of text
# ------------------------------------------------------------------------------

tab <- as.data.frame(data, stringsAsFactors = FALSE)

colnames(tab) <- c("nMay", "Freq")

str(tab)



# ----------
# The mean count of "may" per block text = 0.6565

( lambda <- weighted.mean(as.numeric(tab$nMay), w = tab$Freq) )



# ----------
# Calculate the probabilithies (phat) of k = 0:6 counts, and hence the expected (exp) frequencies in a Poisson distribution

phat <- dpois(0:6, lambda = lambda)

exp <- sum(tab$Freq) * phat

chisq <- (tab$Freq - exp)^2 / exp


( GOF <- data.frame(tab, phat, exp, chisq) )



# ----------
# Chisq = 88.92305 and shows a substantial lack of fit, rejecting the assumptions of the Poisson model.

sum(chisq)

pchisq(sum(chisq), df = nrow(tab) - 2, lower.tail  = FALSE)




# ------------------------------------------------------------------------------
# Use goodfit() to fit the posisson distribution
# ------------------------------------------------------------------------------

data_fit <- vcd::goodfit(data, type = "poisson")



# ----------
# estimated paramters by maximum likelihood estimation
# estimated mean and variance value

unlist(data_fit$par)



# ----------
print(data_fit, digits=2)



# ----------
# The GOF test shows a substantial lack of fit, rejecting the assumptions of the Poisson model.
summary(data_fit)


