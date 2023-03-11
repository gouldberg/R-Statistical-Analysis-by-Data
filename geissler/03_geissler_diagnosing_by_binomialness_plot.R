setwd("//media//kswada//MyFiles//R//geissler")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Geissler
#  - The complete data from Geissler's (1889) tabulation of family sex composition in Saxony.
# ------------------------------------------------------------------------------
data("Geissler", package = "vcdExtra")

data <- Geissler

str(data)


# extract only boys in family with size == 11
data <- data %>% filter(size == 11) %>% dplyr::select(boys, Freq)
data <- xtabs(Freq ~ boys, data = data)



# ------------------------------------------------------------------------------
# Distplot
#
# Hoaglin and Tukey (1985) developed similar plots of a count metameter against k, which appear as a straight line
# when a data distribution follows the general power series family of discrete distribution
#
# For the Binomial distribution, a "binomialness" plot is constructed by plotting log(n(k) / N(n,k)) against k.
# If the points in this plot approximate a stright line, the slope is interpreted as log(p / (1-p)),
# so the binomial parameter p may be estimated as p = exp(b) / ( 1 + exp(b) )
# ------------------------------------------------------------------------------

# binomialness plot shows that heavier tails than in a binomial distributino
distplot(data, type = "binomial", size = 11, xlab = "Number of males children")


distplot(data, type = "nbinomial", size = 11, xlab = "Number of males children")

