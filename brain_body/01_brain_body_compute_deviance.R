setwd("//media//kswada//MyFiles//R//brain_body")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brain and body
#   - Average brain volumes and body masses for seven hominin species
# ------------------------------------------------------------------------------
sppnames <- c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens")

brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)

masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)

d <- data.frame(species = sppnames, brain = brainvolcc, mass = masskg)

str(d)



# ------------------------------------------------------------------------------
# correlation
# ------------------------------------------------------------------------------

plot(d$mass, d$brain, col = rangi2)
text(x = d$mass, y = d$brain + 5, labels = d$species)


cor(d$mass, d$brain)



# ------------------------------------------------------------------------------
# Compute deviance
#   - Deviance is common measure of relative model fit = -2 * sum(log-probability for each observation)
#   - Deviance is an assessment of predictive accuracy,
#     not of truth. The true model, in terms of which predictors are included, is not guaranteed to produce the best predictions.
# ------------------------------------------------------------------------------

# standardize the mass before fitting
d$mass.s <- (d$mass - mean(d$mass)) / sd(d$mass)


mod <- map(
  alist(
    brain ~ dnorm(mu, sigma),
    mu <- a + b * mass.s
  ), 
  data = d,
  start = list(a = mean(d$brain), b = 0, sigma = sd(d$brain)),
  method = "Nelder-Mead"
)


theta <- coef(mod)


# compute deviance:  compute the log-likelihood for each observation amd add them all together.
dev <- (-2) * sum(dnorm(
  d$brain,
  mean = theta[1] + theta[2] * d$mass.s,
  sd = theta[3],
  log = TRUE)
)

dev



# ----------
# compute deviance by cheating
(-2) * logLik(lm(brain ~ mass, d))



