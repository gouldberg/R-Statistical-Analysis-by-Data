setwd("//media//kswada//MyFiles//R//women_queue")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Women in queues
#  - Jinkinson and Slater (1981) and Hoaglin and Tukey (1985) give the frequency distribution of the number of females
#    observed in 100 queues of length 10 in a London Underground station
# ------------------------------------------------------------------------------
data("WomenQueue", package = "vcd")

data <- WomenQueue
data



# ------------------------------------------------------------------------------
# Fit the binomial distribution
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "binomial", par=list(size=10))


# specifying probability paramter = 1/2 in advance
data_fit1 <- vcd::goodfit(data, type = "binomial", par=list(size = 10, prob = 1/2))




# ------------------------------------------------------------------------------
# Rootogram
#
# plot.goodfit() method for "goodfit" objects
# ------------------------------------------------------------------------------
plot(data_fit, type = "hanging", shade = TRUE, xlab = "Number of females")
plot(data_fit1, type = "hanging", shade = TRUE, xlab = "Number of females")


# ----------
# type = "deviation" is better to compare with other models
plot(data_fit, type = "deviation", shade = TRUE, xlab = "Number of females")
plot(data_fit1, type = "deviation", shade = TRUE, xlab = "Number of females")


# -->
# Compared with males, females have more tendency not to line up in a queue if the length of the queue is large ?? 



