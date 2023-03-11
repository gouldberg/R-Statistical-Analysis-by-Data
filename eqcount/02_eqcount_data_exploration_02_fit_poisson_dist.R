setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")


str(EQcount)


EQcount



# ------------------------------------------------------------------------------
# Fit the posisson distribution
#
#  - A naive hypothesis is that annual counts of major earthquakes might follow a Poisson distribution, that is,
#   as independent occurrences with constant probability across years
# ------------------------------------------------------------------------------


tab <- data.frame(table(EQcount))

str(tab)



# ----------
# The mean count of per year = 19.36 ... it is large
( lambda <- mean(EQcount) )
# ( lambda <- weighted.mean(as.numeric(tab$EQcount), w = tab$Freq) )



# ----------
# Calculate the probabilithies (phat) of k = 0:6 counts, and hence the expected (exp) frequencies in a Poisson distribution
phat <- dpois(1:31, lambda = lambda)
exp <- sum(tab$Freq) * phat
chisq <- (tab$Freq - exp)^2 / exp


( GOF <- data.frame(tab, phat, exp, chisq) )




# ----------
# Chisq is large and shows a substantial lack of fit, rejecting the assumptions of the Poisson model.
sum(chisq)

pchisq(sum(chisq), df = nrow(tab) - 2, lower.tail  = FALSE)




# ------------------------------------------------------------------------------
# Use goodfit() to fit the posisson distribution
# ------------------------------------------------------------------------------

data_fit <- vcd::goodfit(table(EQcount), type = "poisson")



# ----------
# estimated paramters by maximum likelihood estimation
# estimated mean and variance value
unlist(data_fit$par)



# ----------
print(data_fit, digits=2)



# ----------
# The GOF test shows a substantial lack of fit, rejecting the assumptions of the Poisson model.
summary(data_fit)


