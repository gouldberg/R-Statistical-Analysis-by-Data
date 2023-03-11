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



# ----------
# convert to table
( tab <- table(PhdPubs$articles) )



# ------------------------------------------------------------------------------
# Fit the poisson and negative binomial distribution to PhdPubs data
# ------------------------------------------------------------------------------

data_fit0 <- vcd::goodfit(tab, type = "poisson")

data_fit1 <- vcd::goodfit(tab, type = "nbinomial")



# ----------
# estimated paramters by maximum likelihood estimation
# Poisson's lambda is close to negative binomial's size
unlist(data_fit0$par)

unlist(data_fit1$par)



# ----------
data_fit0

data_fit1



# ----------
# Both Poisson and negative binomial distribution does not fit good.
summary(data_fit0)

summary(data_fit1)
