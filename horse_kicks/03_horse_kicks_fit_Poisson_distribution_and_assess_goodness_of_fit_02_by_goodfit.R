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
# Use goodfit() to fit the Poisson distribution
#   - goodfit() fits a discrete (count data) distribution for goodness-of-fit tests
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "poisson")


plot(data_fit)



# ----------
# estimated paramters by maximum likelihood estimation
# estimated mean and variance value
unlist(data_fit$par)



# ----------
print(data_fit, digits=0)



# ----------
# Too good to fit
summary(data_fit)


