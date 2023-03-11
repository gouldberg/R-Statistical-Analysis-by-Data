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



# ------------------------------------------------------------------------------
# Aggregated data (do not care about the order of the individual pulls)
# ------------------------------------------------------------------------------

d.aggregated <- aggregate(d$pulled_left, list(prosoc_left = d$prosoc_left, condition = d$condition, actor = d$actor), sum)

d.aggregated

summary(d.aggregated$x)



# ------------------------------------------------------------------------------
# binomial regression by aggregated data
# ------------------------------------------------------------------------------
# Now there are 18 trials on each row, and the likelihood defines the probability of each count * out of 18 trials.
# Also the outcome variable is named as "x" in d.aggregated

mod5 <- map(
  alist(
    x ~ dbinom(18, p),
    logit(p) <- a + (bp + bpC * condition) * prosoc_left,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data = d.aggregated
)


# Note that posterior distribution is the same as the one from model 3
precis(mod5, digits=3)
precis(mod3, digits=3)


# ----------
# to get back onto the probability scale
logistic(c(0.18, 0.32, 0.46))

# --> MAP probability of pulling the left lever is 0.58, with a 89% interval of 0.54 and 0.61
# The chimpanzees exhibited a preference for the left-hand lever, but we will see that individual chimpanzees vary a lot in their handedness,
# so this overall average is misleading.



# ------------------------------------------------------------------------------
# binomial regression:  with one predictor, with interaction
# ------------------------------------------------------------------------------
mod2 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + bp * prosoc_left,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 10)
  ),
  data = d
)


mod3 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + (bp + bpC * condition) * prosoc_left,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data = d
)


# ----------
# The model that includes condition does not do best, but does get more than 25% of the WAIC weight.
compare(mod1, mod2, mod3)



# ----------
# Notice that even though mod2 is not hugely bettern than mod3, the difference has a small standard error.
# Even doubling the standard error to get a 95% interval, the order of the two models would not change.
# So on the basis of information criteria, even though model mod3 of course fits the sample better than model mod2 (because it has more parameters),
# it does not fit sufficiently better to overcome the expected overfitting.
plot(compare(mod1, mod2, mod3))



# ------------------------------------------------------------------------------
# Investigate model 3:  relative effect of size of prosoc_left
# ------------------------------------------------------------------------------
# The estimated interaction effect bpC is negative, with a rather wide posterior on both sides of zero.
# So regardless of the information theory ranking, the estimates suggest that the chimpanzees did not care much about the other animal's presence.
# But they do prefer to pull the prosocial option, as indicated by the estimate for bp.
precis(mod3, corr=TRUE, digits = 3)



# ----------
# the relative effect size of prosoc_left
# proportaional increase of 1.84 in the odds of pulling the left-hand lever, meaning that the odds increase by 84%
# But if the intercept is large enough to guarantee a pull, then increasing the odds by 84% is not going to make ti any more guaranteed.
# Relative effects, as measured by proportional odds or anything else, can be misleading.
exp(0.61)



# ------------------------------------------------------------------------------
# Investigate model 3:  the model-averaged posterior predictive check
#   - in order to get a sense of the absolute effect of each treatment on the probability of pulling the left-hand lever.
# ------------------------------------------------------------------------------

d.pred <- data.frame(
  prosoc_left = c(0, 1, 0, 1),  # right/left/right/left
  condition = c(0, 0, 1, 1)  # control/control/partner/partner
)

# build prediciton ensemble
chimp.ensemble <- ensemble(mod1, mod2, mod3, data = d.pred)

pred.p <- apply(chimp.ensemble$link, 2, mean)
pred.p.PI <- apply(chimp.ensemble$link, 2, PI)


# plot raw data, one trend for each of 7 individual chimpanzees
plot(0, 0, type = "n", xlab = "prosoc_left/condition", ylab = "proportion pulled left", ylim = c(0, 1), xaxt = "n", xlim = c(1,4))
axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))

p <- by(d$pulled_left, list(d$prosoc_left, d$condition, d$actor), mean)
for(chimp in 1:7) lines(1:4, as.vector(p[,,chimp]), col = rangi2, lwd = 1.5)
lines(1:4, pred.p)
shade(pred.p.PI, 1:4)


# -->
# blue lines: the empirical averages for each of the 7 chimps who participated in the experiment
# black line: the average predicted probability of pulling the left-hand lever, across treatments

# Chimps are, at least on average, attracted to the prosocial option.
# But it made little difference whether or not another animal was present to receive the food on the other side of the table.

# There is a lot of variation among individuals in the blue lines.

# Four of the individuals tend to pull the right-hand lever, across all treatments.
# Three individuals tend to pull the left across all treatments.
# One individual, actor number 2, always pulled the left-hand lever, regardless of treatment.


# ------------------------------------------------------------------------------
# Check that the quadratic approximation for the posterior distribution is okay in this case
#   - compare the estimates above to the same model fit using MCMC, via Stan
# ------------------------------------------------------------------------------
# clean NAs from the data
d2 <- d
d2$recipient <- NULL

mod3.stan <- map2stan(mod3, data = d2, iter = 1e4, warmup = 1000)

plot(mod3.stan)



# ----------
precis(mod3)
precis(mod3.stan)


# --> 
# Estimates are almost exactly the same.


# ----------
# The pairs plot confirms that the posterior is multivariate Gaussian
pairs(mod3)
pairs(mod3.stan)

