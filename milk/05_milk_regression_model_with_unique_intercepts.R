setwd("//media//kswada//MyFiles//R//milk")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  milk
# ------------------------------------------------------------------------------
data("milk", package = "rethinking")

d <- milk

dim(d)

str(d)



# ------------------------------------------------------------------------------
# Regression model with unique intercepts:  one parameter for each category
# ------------------------------------------------------------------------------

# Make index variable: which parameter goes with each case
d$clade_id <- coerce_index(d$clade)
d$clade_id



# ----------
mod <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0.6, 10),
    sigma ~ dunif(0, 10)
  ), data = d
)



# ----------
# You'll need to add depth = 2 to the precis call in order to print out vector parameters like these.
precis(mod)
precis(mod, depth = 2)

