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
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ------------------------------------------------------------------------------
# Test of the additional contribution of each 2-way (or higher-way) interations
# ------------------------------------------------------------------------------

add1(mod.nbin, . ~ .^2, test="Chisq")


# -->
# phdprestige:mentor is significant
# and surprisingly other variables are NOT significant !!!



# ----------
# Update the model
mod.nbin2 <- update(mod.nbin, .~.^2)


summary(mod.nbin2)



