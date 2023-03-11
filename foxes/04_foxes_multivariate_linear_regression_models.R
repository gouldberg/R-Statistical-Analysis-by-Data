setwd("//media//kswada//MyFiles//R//foxes")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  foxes
# ------------------------------------------------------------------------------
data("foxes", package = "rethinking")

d <- foxes

dim(d)

str(d)

normalise <- function(x) (x-mean(x))/sd(x)

d <- mutate(d, 
            avgfood.s = normalise(avgfood),
            area.s = normalise(area),
            groupsize.s=normalise(groupsize))



# ------------------------------------------------------------------------------
# Multivariate linear regression
# ------------------------------------------------------------------------------
mod4 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bg * groupsize.s + bf * avgfood.s,
    a ~ dnorm(0, 10),
    bg ~ dnorm(0, 10),
    bf ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data =  d
)


precis(mod4, digits = 3, corr = TRUE)



# ----------
mod5 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba * area.s + bg * groupsize.s + bf * avgfood.s,
    a ~ dnorm(0, 10),
    ba ~ dnorm(0, 10),
    bg ~ dnorm(0, 10),
    bf ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data =  d
)


precis(mod5, digits = 3, corr = TRUE)



# ----------
mod6 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba * area.s + bf * avgfood.s,
    a ~ dnorm(0, 10),
    ba ~ dnorm(0, 10),
    bf ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data =  d
)


# When both avgfood or area are in the same model, their effects are reduced (close to zero) and their standard errors are larger than
# when they are included in separate models

# the posterior MAP of ba and bf is almost mirror image
precis(mod6, digits = 3, corr = TRUE)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
# the models including groupsize and avgfood  (mod4, mod5) are better than other models
compare(mod1, mod2, mod3, mod4, mod5, mod6)

