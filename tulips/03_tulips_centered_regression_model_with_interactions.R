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



# ------------------------------------------------------------------------------
# regression modeling with interactions: centered model
#  - Centering in this analysis will do two thins for us:
#     1. It'll fix our previous problem with maximum iterations (estimation works better)
#     2. It'll make the estimates easier to interpret (estimates changed less across models)
#  - But need to add explicit start lists to each model, because the very flat priors we're using here provide terrible random starting locations.
# ------------------------------------------------------------------------------
d$shade.c <- d$shade - mean(d$shade)
d$water.c <- d$water - mean(d$water)


mod5 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water.c + bS * shade.c,
    a ~ dnorm(130, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  start = list(a = mean(d$blooms), bW = 0, bS = 0, sigma = sd(d$blooms))
)



mod6 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW * water.c + bS * shade.c + bWS * water.c * shade.c,
    a ~ dnorm(130, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  start = list(a = mean(d$blooms), bW = 0, bS = 0, bWS = 0, sigma = sd(d$blooms))
)



# ----------
coeftab(mod5, mod6)

precis(mod5, corr = TRUE, digits = 3)
precis(mod6, corr = TRUE, digits = 3)

compare(mod5, mod6)



# -->
# The intercept now is the grand mean of the outcome variable mean(d$blooms)
# The main effects are the same for both models !!
# The direction of the association for shade does not changed.
# More water appears to directly increase blooms, while more shade directly decreases them.
# Meanwhile, the interaction posterior mean has remained the same as it was in the non-centered model.

# In the un-centered models, the interaction effect is applied to every case, and so none of the parameters in mu makes sense alone.
# This is because neigher of the predictors in those models, shade and water, are ever zero.
# As a result the interaction parameter always factors into generating a prediction.

# The estimate a:  the expected value of blooms when both water and shade are at their average values.
# The estimate bW: the expected change in blooms when water increased by one unit and shade is at its average value (of zero)
# The estimate bS: the expected change in blooms when shade increase by one unit and water is at its average value (of zero)
# The estimate bWS: the expected change in the influence of water on blooms when increasing shade by one unit, and the expected change in the influence of shade on blooms when increasing water by one unit.



# ------------------------------------------------------------------------------
# plotting implied predictions:  visualize interaction effect
# ------------------------------------------------------------------------------
par(mfrow = c(2,3))

shade.seq <- -1:1

# no interaction: mod5
for(w in shade.seq){
  dt <- d[d$water.c == w,]
  plot(blooms ~ shade.c, data = dt, col = rangi2, main = paste("water.c = ", w), xaxp = c(-1, 1, 2), ylim = c(0, 362), xlab = "shade (centered)")
  mu <- link(mod5, data = data.frame(water.c = w, shade.c = shade.seq))
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI, prob = 0.97)
  lines(shade.seq, mu.mean)
  lines(shade.seq, mu.PI[1,], lty = 2)
  lines(shade.seq, mu.PI[2,], lty = 2)
}

# with interaction: mod6
for(w in shade.seq){
  dt <- d[d$water.c == w,]
  plot(blooms ~ shade.c, data = dt, col = rangi2, main = paste("water.c = ", w), xaxp = c(-1, 1, 2), ylim = c(0, 362), xlab = "shade (centered)")
  mu <- link(mod6, data = data.frame(water.c = w, shade.c = shade.seq))
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI, prob = 0.97)
  lines(shade.seq, mu.mean)
  lines(shade.seq, mu.PI[1,], lty = 2)
  lines(shade.seq, mu.PI[2,], lty = 2)
}



# ----------
# When water is at its lowest, on the left, shade has little effect at all, being nearly flat across all values of shade.
# There is a very weak positive trend, but substantial uncertainty about it.

# The likely explanation for these results is that
#  - tulips need both water and light to produce blooms.
#  - at low water levels, shade can not have much of an effect, because the tulips do not have enough water to produce blooms anyway.
#  - at higher water levels, shade can matter more, because the tulips have enough water to produce some blooms.
#  - at very high water levels, water is no longer limiting the blooms very much, and so shade can have a much more dramatic impact on the outcome.


