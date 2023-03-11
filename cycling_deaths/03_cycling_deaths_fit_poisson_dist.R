setwd("//media//kswada//MyFiles//R//cycling_deaths")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  London Cycling Deaths data
# ------------------------------------------------------------------------------
data("CyclingDeaths", package = "vcdExtra")

data <- CyclingDeaths

data

( tab <- table(data$deaths) )



# ------------------------------------------------------------------------------
# Fit the posisson distribution to London Cycling Deaths data
# ------------------------------------------------------------------------------
tab2 <- as.data.frame(tab, stringsAsFactors = FALSE)

colnames(tab2) <- c("nDeath", "Freq")

str(tab2)



# ----------
# The mean count of "may" per block text = 0.5673077
( lambda <- weighted.mean(as.numeric(tab2$nDeath), w = tab2$Freq) )



# ----------
# Calculate the probabilithies (phat) of k = 0:6 counts, and hence the expected (exp) frequencies in a Poisson distribution
phat <- dpois(0:3, lambda = lambda)
exp <- sum(tab2$Freq) * phat
chisq <- (tab2$Freq - exp)^2 / exp


( GOF <- data.frame(tab2, phat, exp, chisq) )



# ----------
# Chisq = 2.97 and shows not rejecting the assumptions of the Poisson model.
sum(chisq)

pchisq(sum(chisq), df = nrow(tab2) - 2, lower.tail  = FALSE)



# ------------------------------------------------------------------------------
# Use goodfit() to fit the posisson distribution
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(tab, type = "poisson")



# ----------
# estimated paramters by maximum likelihood estimation
# estimated mean and variance value
unlist(data_fit$par)



# ----------
print(data_fit, digits=2)



# ----------
# The GOF test shows not rejecting the assumptions of the Poisson model.
summary(data_fit)


