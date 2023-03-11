setwd("//media//kswada//MyFiles//R//chimpanzees")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prosocial chimpanzees
# ------------------------------------------------------------------------------
data("chimpanzees", package = "rethinking")

d <- chimpanzees

dim(d)

str(d)


# ----------
# clean NAs from the data
d2 <- d
d2$recipient <- NULL


unique(d$actor)



# ------------------------------------------------------------------------------
# model with varying intercepts on actor, but not yet on block
# ------------------------------------------------------------------------------

mod5 <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + a_actor[actor] + (bp + bpc * condition) * prosoc_left,
    a_actor[actor] ~ dnorm(0, sigma_actor),
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpc ~ dnorm(0, 10),
    sigma_actor ~ dcauchy(0, 1)
  ),
  data = d, chains = 4, iter = 5e3, warmup = 1e3, cores = 10
)


plot(mod5)

precis(mod5, digits=3, depth = 2)



# ----------
# compute total intercept for each actor
post <- extract.samples(mod5)
total_a_actor <- sapply(1:7, function(actor) post$a + post$a_actor[,actor])
round(apply(total_a_actor, 2, mean), 2)



# ------------------------------------------------------------------------------
# model with two types of cluster: actor and block
# ------------------------------------------------------------------------------
d$block_id <- d$block  # name 'block' is reserved by Stan

str(d$block_id)


mod6 <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + a_actor[actor] + a_block[block_id] + (bp + bpc * condition) * prosoc_left,
    a_actor[actor] ~ dnorm(0, sigma_actor),
    a_block[block_id] ~ dnorm(0, sigma_block),
    c(a, bp, bpc) ~ dnorm(0, 10),
    sigma_actor ~ dcauchy(0, 1),
    sigma_block ~ dcauchy(0, 1)
  ),
  data = d, chains = 4, iter = 6e3, warmup = 1e3, cores = 10
)


plot(mod6)

precis(mod6, digits=3, depth = 2)


# ----------
par(mfrow=c(1,1))
plot(precis(mod6, depth = 2))



# -->
# Notice that the number of effective samples, n_eff, varies quite a lot across parameters.
# This is common in complex model. In this sort of model the most common reason is that some parameters spends a lot of time near a boundary.
# The parameter simga_block spends a lot of time near its minimum of zero. As a consequence, you may also see a warning about "divergent iterations"


# Comparing simga_actor to sigma_block and notice that the estimated variation among actors is a lot larger than the estimated variation among blocks.


# ----------
# plot the marginal posterior distribution of these two paramters
post <- extract.samples(mod6)
dens(post$sigma_block, xlab = "sigma", xlim = c(0, 4))
dens(post$sigma_actor, col = rangi2, lwd = 2, add = TRUE)
text(2, 0.85, "actor", col = rangi2)
text(0.75, 2, "block")


summary(coef(mod6)[1:7])
summary(coef(mod6)[8:13])


# -->
# adding block to this model has not added a lot of overfitting risk.


# ----------
compare(mod5, mod6)


# -->
# Look at the pWAIC column, which reports the "effective number of parameters". While mod6 has 7 more parameters (by block) than mod5,
# it has only about 2.5 more effective parameters, because the posterior distribution for sigma_block ended up close to zero.
# This means each of the 6 a_block parameters is strongly shrunk towards zero, they are relatively inflexbible.

# Also notice that the difference in WAIC between these models is small, only 1.2.
# This is especially smalle compared the standard deviaion of the difference, 1.94.
# These two models imply nearly identical predictions.
# THe block parameters have been shrunk so much towards zero that they do very little work in the model.

#### IMPORTANT ####
# You can think of the sigma parameter for each cluster as a crude measure of the cluster's relevance for explaining variation in the outcome.


# ------------------------------------------------------------------------------
# plot posterior predictions, not including block, for mod5 (not mod6)
# ------------------------------------------------------------------------------
graphics.off()
par(mfrow=c(3,3))


# blue line: empirical proportion of left pulls in each treatment for a single individual
# black line and shaded region: the average predicted probability and its 89% interval.

for(chimp in 1:7){
  
  d.pred <- list(
    prosoc_left = c(0, 1, 0, 1), # right/left/right/left
    condition = c(0, 0, 1, 1),  # control/control/partner/partner
    actor = rep(chimp, 4)
  )
  
  
  link.mod5 <- link(mod5, data = d.pred)
  pred.p <- apply(link.mod5, 2, mean)
  pred.p.PI <- apply(link.mod5, 2, PI)
  
  
  # plot raw data, one trend for each of 7 individual chimpanzees
  plot(0, 0, type = "n", xlab = "prosoc_left/condition", ylab = "proportion pulled left", ylim = c(0, 1), xlim = c(1,4), yaxp = c(0, 1, 2))
  axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))
  mtext(paste("actor", chimp))
  
  p <- by(d$pulled_left, list(d$prosoc_left, d$condition, d$actor), mean)
  lines(1:4, as.vector(p[,,chimp]), col = rangi2, lwd = 2)
  lines(1:4, pred.p)
  shade(pred.p.PI, 1:4)
}



# ------------------------------------------------------------------------------
# plot posterior predictions for new clusters for mod5 (not mod6)
#  - a_actor = 0
#  - includes a_actor
#  - simulate actors
# ------------------------------------------------------------------------------
graphics.off()
par(mfrow=c(1,3))


# ----------
d.pred <- list(
  prosoc_left = c(0, 1, 0, 1), # right/left/right/left
  condition = c(0, 0, 1, 1),  # control/control/partner/partner
  actor = rep(2, 4)  # just placeholder
)


# ----------
# replace varying intercept samples with zeros
a_actor_zeros <- matrix(0, 1000, 7)
link.mod5_1 <- link(mod5, n = 1000, data = d.pred, replace = list(a_actor = a_actor_zeros))
pred.p.mean_1 <- apply(link.mod5_1, 2, mean)
pred.p.PI_1 <- apply(link.mod5_1, 2, PI, prob = 0.8)


# ----------
# replace varying intercept samples with simulation
post <- extract.samples(mod5)
a_actor_sims <- rnorm(7000, 0, post$sigma_actor)
a_actor_sims <- matrix(a_actor_sims, 1000, 7)
link.mod5_2 <- link(mod5, n = 1000, data = d.pred, replace = list(a_actor = a_actor_sims))
pred.p.mean_2 <- apply(link.mod5_2, 2, mean)
pred.p.PI_2 <- apply(link.mod5_2, 2, PI, prob = 0.8)


# ----------
# simulate a new actor from the estimated population of actors
post <- extract.samples(mod5)
sim.actor <- function(i){
  sim_a_actor <- rnorm(1, 0, post$sigma_actor[i])
  p <- c(0, 1, 0, 1)
  C <- c(0, 0, 1, 1)
  p <- logistic(post$a[i] + sim_a_actor + (post$bp[i] + post$bpc[i] * C) * p)
  return(p)
}


# ----------
# solid line: posterior means and the shaded regions are 80% percentile intervals

# predictions for an average actor (varying intercept a_actor = 0)
plot(0, 0, type = "n", xlab = "prosoc_left/condition", ylab = "proportion pulled left", ylim = c(0, 1), xlim = c(1,4))
axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))
mtext("average actor with a_actor = 0")
lines(1:4, pred.p.mean_1)
shade(pred.p.PI_1, 1:4)


# simulated varying intercepts using the posterior standard deviaion among actors
plot(0, 0, type = "n", xlab = "prosoc_left/condition", ylab = "proportion pulled left", ylim = c(0, 1), xlim = c(1,4))
axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))
mtext("average actor with a_actor")
lines(1:4, pred.p.mean_2)
shade(pred.p.PI_2, 1:4)


# plot 50 simulate actors
plot(0, 0, type = "n", xlab = "prosoc_left/condition", ylab = "proportion pulled left", ylim = c(0, 1), xlim = c(1,4))
axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))
mtext("simulated actors")
for(i in 1:50) lines(1:4, sim.actor(i), col=col.alpha("black", 0.5))


# -->
# Note the interaction of treatment and the variation among actors.
# Because this is a binomial model, in principle all parameters interact, due to ceiling and floor effects.
# For actors with very large intercepts, near the top of the plot, treatment has very little effect.
# There actors have strong handedness preferences. But actors with intercepts nearer the mean are influenced by treatment.

