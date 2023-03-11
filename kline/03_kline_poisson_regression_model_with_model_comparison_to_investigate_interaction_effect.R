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



# ------------------------------------------------------------------------------
# model investigation: model comparison
#  - A better way to assess whether a predictor, like contact_high, is expected to improve prediction is to use model comparison
#    Since model comparisons are done on the scale of predicted outcomes, they automatically take account of these correlations
# ------------------------------------------------------------------------------

# no interaction
mod2 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp * log_pop + bc * contact_high,
    a ~ dnorm(0, 100),
    c(bp, bc) ~ dnorm(0, 1)
  ),
  data = d
)


# no contact rate
mod3 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp * log_pop,
    a ~ dnorm(0, 100),
    bp ~ dnorm(0, 1)
  ),
  data = d
)



# no log-population
mod4 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bc * contact_high,
    a ~ dnorm(0, 100),
    bc ~ dnorm(0, 1)
  ),
  data = d
)


# intercept only (null model)
mod5 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(0, 100)
  ),
  data = d
)


# ----------
# compare all using WAIC
# adding n = 1e4 for more stable WAIC estimates
islands.compare <- compare(mod1, mod2, mod3, mod4, mod5, n = 1e4)
islands.compare

plot(islands.compare)


# -->
# The top two models include both predictors, but the top model mod2, excludes the interaction between them.
# THere's a lot of model weight assigned to both, however.
# This suggetsts the interaction estimate is probably overfit.
# This model set is decent evidence that contact rate matters, although it influences prediction much less than log-population does.



# ------------------------------------------------------------------------------
# model investigation: counterfactual predictions using a ensemble of the top 3 models
# ------------------------------------------------------------------------------

pch <- ifelse(d$contact_high == 1, 16, 1)
plot(d$log_pop, d$total_tools, col = rangi2, pch = pch, xlab = "log_population", ylab = "total tools")

log_pop.seq <- seq(from = 6, to = 13, length.out = 30)


d.pred <- data.frame(log_pop = log_pop.seq, contact_high = 1)
lambda.pred.h <- ensemble(mod1, mod2, mod3, data = d.pred)
lambda.med <- apply(lambda.pred.h$link, 2, median)
lambda.PI <- apply(lambda.pred.h$link, 2, PI)
lines(log_pop.seq, lambda.med, col = rangi2)
shade(lambda.PI, log_pop.seq, col = col.alpha(rangi2, 0.2))

d.pred <- data.frame(log_pop = log_pop.seq, contact_high = 0)
lambda.pred.l <- ensemble(mod1, mod2, mod3, data = d.pred)
lambda.med <- apply(lambda.pred.l$link, 2, median)
lambda.PI <- apply(lambda.pred.l$link, 2, PI)
lines(log_pop.seq, lambda.med, col = rangi2)
shade(lambda.PI, log_pop.seq, col = col.alpha("black", 0.1))


# -->
# Notice that both trends curve dramatically upwards as log-population increases.
# The impact of contact rate can bes seen by the distance between the blue and gray predictions.
# There is plenty of overlap, especially at low nad high log-population values, where there are no islands with high contact rate.



