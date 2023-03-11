setwd("//media//kswada//MyFiles//R//tulips")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tulips
# ------------------------------------------------------------------------------
data("tulips", package = "rethinking")

d <- tulips

dim(d)

str(d)

d$shade.c <- d$shade - mean(d$shade)
d$water.c <- d$water - mean(d$water)



# ------------------------------------------------------------------------------
# include "bed" as main effect (categorical variable)
# ------------------------------------------------------------------------------
table(d$bed)


# ----------
# by dummy variable
d$bed.a <- ifelse(d$bed == "a", 1, 0)
d$bed.b <- ifelse(d$bed == "b", 1, 0)
d$bed.c <- ifelse(d$bed == "a", 1, 0)


mod7 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + ba * bed.a + bb * bed.b + bc * bed.c + bW * water.c + bS * shade.c + bWS * water.c * shade.c,
    a ~ dnorm(130, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    c(ba, bb, bc) ~ dnorm(0, 10),  # does not work by dnorm(0, 100)
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  start = list(a = mean(d$blooms), bW = 0, bS = 0, bWS = 0, sigma = sd(d$blooms))
)



# ---------
# by index variable
d$bed_id <- coerce_index(d$bed)

mod8 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a[bed_id] + bW * water.c + bS * shade.c + bWS * water.c * shade.c,
    a[bed_id] ~ dnorm(130, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  start = list(a = mean(d$blooms), bW = 0, bS = 0, bWS = 0, sigma = sd(d$blooms)),
  method = "Nelder-Mead", control = list(maxit=1e4)  # this is required !!!
)


# ----------
coeftab(mod6, mod7, mod8)

precis(mod7, digits = 3)
precis(mod8, depth = 2, digits = 3)
precis(mod6, digits = 3)


# -->
# bW, bS, bWS are not much different, but a and sigma is different.
# sigma in mod8 is smallest
# the mod8 (including bed as index) is better estimation in terms of WAIC.



compare(mod6, mod7, mod8)


