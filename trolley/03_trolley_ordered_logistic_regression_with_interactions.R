setwd("//media//kswada//MyFiles//R//trolley")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Trolley
# ------------------------------------------------------------------------------
data("Trolley", package = "rethinking")

d <- Trolley

dim(d)

str(d)



# ------------------------------------------------------------------------------
# Why we subtract phi, the linear model beta * x, from each intercept, rather than add it ?
# ------------------------------------------------------------------------------
# The function dordlogit makes the calculation of likelihoods straightforward
( pk <- dordlogit(1:7, 0, coef(mod1)) )

# these probabilities imply an average outcome value of
sum(pk * (1:7))


# Suppose we take the MAP estimates from mod1 and subtract 0.5 from each.
( pk <- dordlogit(1:7, 0, coef(mod1) - 0.5) )
sum(pk * (1:7))


# -->
# a positive beta value indicates that an increase in the predictor variable x results in an increase in the average response.



# ------------------------------------------------------------------------------
# We model the log-odds of each possible response to be an additive model of the features of the story corresponding to each response
# ------------------------------------------------------------------------------
# Notice that there is no link function around phi, because the link is really inside dordlogit already. ("logit")
# Notice also that we have adopted the approximate MAP estimates from the previous model (intercept only), mod1, as starting values for the intercepts.

mod2 <- map(
  alist(
    response ~ dordlogit(phi, c(a1, a2, a3, a4, a5, a6)),
    phi <- bA * action + bI * intention + bC * contact,
    c(bA, bI, bC) ~ dnorm(0, 10),
    c(a1, a2, a3, a4, a5, a6) ~ dnorm(0, 10)
  ),
  data = d,
  start = list(a1 = -1.9, a2 = -1.2, a3 = -0.7, a4 = 0.2, a5 = 0.9, a6 = 1.8)
)



# The variables action and contact cannot interact, because contact is just a type of action

mod3 <- map(
  alist(
    response ~ dordlogit(phi, c(a1, a2, a3, a4, a5, a6)),
    phi <- bA * action + bI * intention + bC * contact + bAI * action * intention + bCI * contact * intention,
    c(bA, bI, bC, bAI, bCI) ~ dnorm(0, 10),
    c(a1, a2, a3, a4, a5, a6) ~ dnorm(0, 10)
  ),
  data = d,
  start = list(a1 = -1.9, a2 = -1.2, a3 = -0.7, a4 = 0.2, a5 = 0.9, a6 = 1.8)
)



# ----------
precis(mod2, digits=3)
precis(mod3, digits=3)


# -->
# all of the slope estimates (3 main effets and 2 interactions) are quite reliably negative.
# all of the slopes are negative, which implies that each factor/interaction reduces the average response
# including action, intention or contact in a story leads people to judge it as less morally permissible.


# ----------
coeftab(mod1, mod2, mod3)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
compare(mod1, mod2, mod3, refresh = 0.1)


# --> mod3 absolutely dominates based upon WAIC.
# We can safely proceed here, ignoring model uncertainty.  (no need to ensemble)


# ----------
# plot implied predictions
post <-  extract.samples(mod3)


# ----------
n_samp <- 1000
par(mfrow=c(1,3))

kA <- 0;  kC <- 0;  kI <- 0:1
plot(1, 1, type = "n", xlab = "intention", ylab = "probability", xlim = c(0, 1), ylim = c(0, 1), xaxp = c(0, 1, 1), yaxp = c(0, 1, 2))

# pordlogit computes cumulative ordered logit probabilities
for(s in 1:n_samp){
  p <- post[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bA * kA + p$bI * kI + p$bC * kC + p$bAI * kA * kI + p$bCI * kC * kI
  pk <- pordlogit(1:6, a = ak, phi = phi)
  for(i in 1:6) lines(kI, pk[,i], col = col.alpha(rangi2, 0.1))
}
mtext(concat("action=", kA, ", contact=", kC))


kA <- 1;  kC <- 0;  kI <- 0:1
plot(1, 1, type = "n", xlab = "intention", ylab = "probability", xlim = c(0, 1), ylim = c(0, 1), xaxp = c(0, 1, 1), yaxp = c(0, 1, 2))
for(s in 1:n_samp){
  p <- post[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bA * kA + p$bI * kI + p$bC * kC + p$bAI * kA * kI + p$bCI * kC * kI
  pk <- pordlogit(1:6, a = ak, phi = phi)
  for(i in 1:6) lines(kI, pk[,i], col = col.alpha(rangi2, 0.1))
}
mtext(concat("action=", kA, ", contact=", kC))


kA <- 0;  kC <- 1;  kI <- 0:1
plot(1, 1, type = "n", xlab = "intention", ylab = "probability", xlim = c(0, 1), ylim = c(0, 1), xaxp = c(0, 1, 1), yaxp = c(0, 1, 2))
for(s in 1:n_samp){
  p <- post[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bA * kA + p$bI * kI + p$bC * kC + p$bAI * kA * kI + p$bCI * kC * kI
  pk <- pordlogit(1:6, a = ak, phi = phi)
  for(i in 1:6) lines(kI, pk[,i], col = col.alpha(rangi2, 0.1))
}
mtext(concat("action=", kA, ", contact=", kC))


# -->
# blue line:  the boundaries between response values numbered 1 through 7
# The thickness of the blue lines corresponds to the variation in predictions due to variation in samples from the posterior.
# This plot shows the large interaction effect between contact and intension, the largest estimated effect in the model.


