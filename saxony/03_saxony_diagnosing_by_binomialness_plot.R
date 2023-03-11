setwd("//media//kswada//MyFiles//R//saxony")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Saxnony
#  - The nmber of male children in families of size 12 in Saxony
# ------------------------------------------------------------------------------
data("Saxony", package = "vcd")

data <- Saxony



# ------------------------------------------------------------------------------
# Fit the binomial distribution to Saxony data
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "binomial", par=list(size=12))



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
distplot(data, type = "binomial", size = 12, xlab = "Number of males")


# -->
# It is necessary to question the assumptions of the binomial (births of males are independent Bernoulli trials with constant probability p)
# as a model for this birth distribution and/or find a more adequate model.

# On these questions, Edwards (1958) reviews numerous other studies of these Geissler's data, and fits a so-called beta-binomial model
# proposed by Skellam (1948), where p varies among families according to a beta distribution.
# He concludes that there is evidence that p varies between families of the same size.
# One suggested explanation is that family decisions to have a further child is influenced by the balance of boys and girls among their earlier children.
