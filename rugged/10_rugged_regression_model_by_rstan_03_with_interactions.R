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
# regression modeling with interactions
# ------------------------------------------------------------------------------

mod5 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + gamma * rugged + bA * cont_africa,
    gamma <- bR + bAR * cont_africa,
    a ~ dnorm(8, 100),
    bA ~ dnorm(0, 1),
    bR ~ dnorm(0, 1),
    bAR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = dd
)



# ----------
# Since gamma does not appear in this table (it was not estimated), we have to comput it ourselves.
precis(mod5, corr = TRUE, digits = 3)




# ----------
compare(mod3, mod4, mod5)



# -->
# mod5 has about 97% of the WAIC-estimated model weight. That's very strong support for including the interaction effect.
# That probably isn't surprising, given the obvious difference in slope.
# But the modicum of weight given to mod4 suggests that the posterior means for the slopes in mod5 are a little overfit.
# And the standard error of the difference in WAIC between the top two models is almost the same as the difference itself.
# There are only so many African countries, after all, so the data are sparse as far as estimating the interaction goes.
