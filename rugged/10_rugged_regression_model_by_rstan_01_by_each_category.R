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
# regression modeling by category
# ------------------------------------------------------------------------------
# African nations and non-African nations

mod1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = d.A1
)


mod2 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = d.A0
)



# ----------
precis(mod1, corr=TRUE, digits = 3)


precis(mod2, corr=TRUE, digits = 3)

