setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ------------------------------------------------------------------------------
# Check mean and variance
# ------------------------------------------------------------------------------

mean(PhdPubs$articles)

var(PhdPubs$articles)



# -->
# Mean is less than variance
# The Poisson distribution does not fit to this data.
# We may try fitting by negative binomial or geometric distribution to this data.
