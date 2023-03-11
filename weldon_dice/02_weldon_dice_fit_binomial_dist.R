setwd("//media//kswada//MyFiles//R//weldon_dice")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Weldon's dice
# ------------------------------------------------------------------------------
data("WeldonDice", package = "vcd")

data <- WeldonDice

data
sum(data)


dimnames(data)$n56[11] <- "10+"



# ------------------------------------------------------------------------------
# Fit the binomial distribution to Weldon's dicce data
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "binomial", par = list(size = 12))


# ----------
# estimated paramters by maximum likelihood estimation
# the probability of a success (a 5 or 6) is estimated as p = 0.3377, not far from the theoretical value p = 1/3
unlist(data_fit$par)



# ----------
print(data_fit, digits=0)



# ----------
# acceptable fit for the binomial distribution
summary(data_fit)
