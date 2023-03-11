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
# Check mean and variance of London Cycling Deaths data
# ------------------------------------------------------------------------------
with(data, c(mean = mean(deaths), var=var(deaths), ratio = mean(deaths) / var(deaths)))


# -->
# no evidence for over- or underdispersion.



# ------------------------------------------------------------------------------
# Calculating the probability of more than 5 deaths in two-week period
# ------------------------------------------------------------------------------
# Whether it was an extraordinary event to observe 6 deaths in a two-week period, by calculating the probability of more than 5 deaths using ppois()
ppois(5, mean(data$deaths), lower.tail = FALSE)


# -->
# This probability is extremely small.
# So we conclude that the occurrence of six deaths was singlar event.



