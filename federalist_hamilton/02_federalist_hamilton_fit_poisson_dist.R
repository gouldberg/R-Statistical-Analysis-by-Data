setwd("//media//kswada//MyFiles//R//federalist_hamilton")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Federalist Papers  Hamilton
# ------------------------------------------------------------------------------
data <- c(129, 83, 20, 9, 5, 1)
names(data) <- 0:5

data <- as.table(data)




# ------------------------------------------------------------------------------
# Fit the posisson distribution to Federalist Papers data
#
#  - A naive hypothesis is that thesse occurrences might follow a Poisson distribution, that is,
#   as independent occurrences with constant probability across the 247 blocks of text
# ------------------------------------------------------------------------------
# The mean count of "upon" per block text = 0.708502
( lambda <- weighted.mean(as.numeric(names(data)), w = data) )


# ----------
# Calculate the probabilithies (phat) of k = 0:6 counts, and hence the expected (exp) frequencies in a Poisson distribution
phat <- dpois(0:5, lambda = lambda)
exp <- sum(data) * phat
chisq <- as.numeric((data - exp)^2 / exp)


( GOF <- data.frame(nUpon = as.numeric(data), phat, exp, chisq) )



# ----------
# Chisq = 19.20181 and shows a substantial lack of fit, rejecting the assumptions of the Poisson model.
sum(chisq)

pchisq(sum(chisq), df = nrow(data) - 2, lower.tail  = FALSE)



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


