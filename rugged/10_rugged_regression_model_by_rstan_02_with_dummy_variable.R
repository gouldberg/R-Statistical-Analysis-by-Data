setwd("//media//kswada//MyFiles//R//rugged")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rugged
# ------------------------------------------------------------------------------

data("rugged", package = "rethinking")

d <- rugged

dim(d)

str(d)



# ----------
# make log version of outcome
d$log_gdp <- log(d$rgdppc_2000)


# extract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000), ]


# split countries into Africa and not-Africa
d.A1 <- dd[dd$cont_africa == 1, ]
d.A0 <- dd[dd$cont_africa == 0, ]


summary(d.A1$rugged)
summary(d.A0$rugged)



# ------------------------------------------------------------------------------
# regression modeling with dummy variable
# ------------------------------------------------------------------------------

# entire data (Africa and non-Africa) in one model
mod3 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = dd
)



# Africa and non-Africa in same model + dummy variable for category
mod4 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged + bA * cont_africa,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = dd
)



# ----------
precis(mod3, corr=TRUE, digits = 3)



precis(mod4, corr=TRUE, digits = 3)



# ----------
compare(mod3, mod4)



# -->
# This is a case in which it will be safe to ignore the lower ranked model, as model 4 takes all the model weight.
# And while the standard error of the difference in WAIC is 15, the difference itself is 63, implying a 95% interval of 63 +- 30.
# So the continent predictor variable seems to be picking up something important to the sample, even accounting for expected overfitting.

