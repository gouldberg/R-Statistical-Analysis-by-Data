setwd("//media//kswada//MyFiles//R//kline")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Kline
# ------------------------------------------------------------------------------
data("Kline", package = "rethinking")

d <- Kline

dim(d)

str(d)


# ----------
# Theory says that it is the order of magnitude of the population that matters, not the absolute size of it.
d$log_pop <- log(d$population)
d$contact_high <- ifelse(d$contact == "high", 1, 0)

d$society <- 1:10



# ------------------------------------------------------------------------------
# poisson regresion with varying intercepts (a_society), to handle overdispersion
# ------------------------------------------------------------------------------
mod2 <- map2stan(
  alist(
    total_tools ~ dpois(mu),
    log(mu) <- a + a_society[society] + bp * log_pop,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 1),
    a_society[society] ~ dnorm(0, sigma_society),
    sigma_society ~ dcauchy(0, 1)
  ),
  data = d, iter = 4e3, chains = 3, cores = 10
)


precis(mod2, digits = 3, depth = 2)


# -->
# This model samples very efficiently, despite using 13 parameters to describe 10 observations
# Remember: varying effect parameters are adaptively regularized. So they are not completely flexible and induce much less overfitting risk.


# ----------
WAIC(mod2)

# --> WAIC tells that effective number of parameters is about 5



# ------------------------------------------------------------------------------
# Visualize the over-dispersion
# ------------------------------------------------------------------------------

post <- extract.samples(mod2)

d.pred <- list(
  log_pop = seq(from = 6, to = 14, length.out = 30),
  society = rep(1, 30)
)

a_society_sims <- rnorm(20000, 0, post$sigma_society)
a_society_sims <- matrix(a_society_sims, 2000, 10)
link.mod2 <- link(mod2, n = 2000, data = d.pred, replace = list(a_society = a_society_sims))


# ----------
par(mfrow=c(1,1))
plot(d$log_pop, d$total_tools, col = rangi2, pch = 16, xlab = "log population", ylab = "total tools")

mu.median <- apply(link.mod2, 2, median)
lines(d.pred$log_pop, mu.median)

mu.PI <- apply(link.mod2, 2, PI, prob = 0.97)
shade(mu.PI, d.pred$log_pop)

mu.PI <- apply(link.mod2, 2, PI, prob = 0.89)
shade(mu.PI, d.pred$log_pop)

mu.PI <- apply(link.mod2, 2, PI, prob = 0.67)
shade(mu.PI, d.pred$log_pop)


# -->
# The envelope of predictions is a lot wider here than previous model (mod1)
# This is a consequence of the varying intercepts, combined with the fact that there is much more variation in the data than a pure-Poisson model anticipates.
