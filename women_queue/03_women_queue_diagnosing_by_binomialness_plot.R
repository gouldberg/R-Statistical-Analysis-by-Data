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
# Distplot
#
# Hoaglin and Tukey (1985) developed similar plots of a count metameter against k, which appear as a straight line
# when a data distribution follows the general power series family of discrete distribution
#
# For the Binomial distribution, a "binomialness" plot is constructed by plotting log(n(k) / N(n,k)) against k.
# If the points in this plot approximate a stright line, the slope is interpreted as log(p / (1-p)),
# so the binomial parameter p may be estimated as p = exp(b) / ( 1 + exp(b) )
#
# In this plot, the open circles show the calculated observed values of the count Metameter.
# The smaller filled points show the centers of the confidence intervals CI.center, and the dashed lines show the extent of the confidence intervals
# ------------------------------------------------------------------------------

# binomialness plot shows that significant departures heavier tails than in a binomial distributino
distplot(data, type = "binomial", size = 10, xlab = "Number of females")

# distplot(data, type = "nbinomial", size = 10, xlab = "Number of females")


# -->
# The data has significant departure from binomial distribution