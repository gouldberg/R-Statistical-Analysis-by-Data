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
# Fit the negative binomial distribution
#
#  - The negative binomial distribution is a type of waiting-time distribution, but also arises in statistical applications
#   as a generalization of the Poisson distribution, allowing for overdispersion (variance > mean).
#   One form of the negative binomial distribution (also called the Pascal distribution) arises when a series of independent Bernoulli trials
#   is observed with constant probability p of some event, and we ask how many non-events (failures), k, it takes to observe n successfule events.
# ------------------------------------------------------------------------------
# firstly, poisson distribution for comparison
data_fit0 <- vcd::goodfit(data, type = "poisson")

data_fit1 <- vcd::goodfit(data, type = "nbinomial")



# ----------
# estimated paramters by maximum likelihood estimation
unlist(data_fit1$par)



# ----------
# Better fit than poisson distribution
print(data_fit0, digits=2)
print(data_fit1, digits=2)



# ----------
# The GOF test shows, not fit to Poisson distribution, but may fit to negative binomial distribution
summary(data_fit0)
summary(data_fit1)


