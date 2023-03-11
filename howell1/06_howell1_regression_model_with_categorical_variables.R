setwd("//media//kswada//MyFiles//R//howell1")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Howell1
# ------------------------------------------------------------------------------
data("Howell1", package = "rethinking")

d <- Howell1

dim(d)

str(d)



# ------------------------------------------------------------------------------
# regression model with only 1 category variable (male)
# ------------------------------------------------------------------------------
str(d$male)

mod <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm * male,
    a ~ dnorm(178, 100),
    bm ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d)


precis(mod, corr = TRUE)


# -->
# The expected average female height is 135 cm.
# The average difference between males and female, 7.3 cm.
# So to compute the average male height, you just add these two estimates: 135 + 7.3 = 142.3 cm



# ----------
# The width of the posterior distribution, sampling from posterior distribution
post <- extract.samples(mod)
mu.male <- post$a + post$bm
PI(mu.male)
