setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ----------
d$height_normalised <- (d$height - mean(d$height))/sd(d$height)



# ------------------------------------------------------------------------------
# model with varying intercepts and slopes (on age), clustered by subject
# ------------------------------------------------------------------------------

mod.centered <- map2stan(
  alist(
    height_normalised ~ dnorm(mu, sigma), #predict normalised height to be able to compare intercept and slope on the same scale
    mu <- a + a_individual[Subject] + (b_age + b_age_individual[Subject]) * age,
    c(a_individual, b_age_individual)[Subject] ~ dmvnorm2(0, sigma_ind, Rho),
    a ~ dnorm(0, 100),
    b_age ~ dnorm(0, 1),
    sigma_ind ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2),
    sigma ~ dcauchy(0, 2)
  ),
  data=d,
  warmup=1000, iter=3000, chains=2, cores=10
)



# ----------

precis(mod.centered, depth=2)


# Number of effective samples for a_individual and b_age_individual are really small (~200)
# I've found that moving parameter 'a' and 'b_age' into the dmvnorm2 makes inference faster and lead to much better n_eff.
# The problem with it, that is no longer possible to compare a_individual to b_age_individual to determine which one has greater influence on the result.


