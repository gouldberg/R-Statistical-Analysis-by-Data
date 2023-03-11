setwd("//media//kswada//MyFiles//R//foxes")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  foxes
# ------------------------------------------------------------------------------
data("foxes", package = "rethinking")

d <- foxes

dim(d)

str(d)

normalise <- function(x) (x-mean(x))/sd(x)

d <- mutate(d, 
            avgfood.s = normalise(avgfood),
            area.s = normalise(area),
            groupsize.s=normalise(groupsize))


# ------------------------------------------------------------------------------
# Regression by each variables
# ------------------------------------------------------------------------------
# body weight as a linear function of territory size
mod1 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba * area.s,
    a ~ dnorm(0, 10),
    ba ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data =  d
)


precis(mod1, digits = 3, corr = TRUE)



# body weight as a linear function of group size
mod2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bg * groupsize.s,
    a ~ dnorm(0, 10),
    bg ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data =  d
)


precis(mod2, digits = 3, corr = TRUE)



# ------------------------------------------------------------------------------
# plot regression lines from posterior distribution
# ------------------------------------------------------------------------------
post_1 <- extract.samples(mod1)
post_2 <- extract.samples(mod2)


par(mfrow=c(1,2))
plot(d$area.s, d$weight, xlim=range(d$area.s), ylim=range(d$weight), col = rangi2, xlab = "territory size", ylab = "weight")
for(i in 1:20) abline(a = post_1$a[i], b = post_1$b[i], col = col.alpha("black", 0.3))

plot(d$groupsize.s, d$weight, xlim=range(d$groupsize.s), ylim=range(d$weight), col = rangi2, xlab = "group size", ylab = "weight")
for(i in 1:20) abline(a = post_2$a[i], b = post_2$b[i], col = col.alpha("black", 0.3))



# ------------------------------------------------------------------------------
# plot raw data and counterfactual plot
# ------------------------------------------------------------------------------
area.seq <- seq(from=-3, to = 3, by = 0.1)

mu1 <- link(mod1, data = data.frame(area.s = area.seq))
sim.weight1 <- sim(mod1, data = list(area.s = area.seq), n = 1e3)
mu1.mean <- apply(mu1, 2, mean)
mu1.HPDI <- apply(mu1, 2, HPDI, prob = 0.89)
weight1.PI <- apply(sim.weight1, 2, PI, prob = 0.89)


gsize.seq <- seq(from=-3, to = 3, by = 0.1)

mu2 <- link(mod2, data = data.frame(groupsize.s = gsize.seq))
sim.weight2 <- sim(mod2, data = list(groupsize.s = gsize.seq), n = 1e3)
mu2.mean <- apply(mu2, 2, mean)
mu2.HPDI <- apply(mu2, 2, HPDI, prob = 0.89)
mu2.PI <- apply(mu2, 2, PI, prob = 0.95)
weight2.PI <- apply(sim.weight2, 2, PI, prob = 0.89)



# ----------
par(mfrow=c(1,2))
plot(d$weight ~ d$area.s, xlim=range(d$area.s), ylim=range(d$weight), col = rangi2, xlab = "territory size", ylab = "weight")
lines(area.seq, mu1.mean)
shade(mu1.HPDI, area.seq)
shade(weight1.PI, area.seq)

plot(d$weight ~ d$groupsize.s, xlim=range(d$groupsize.s), ylim=range(d$weight), col = rangi2, xlab = "group size", ylab = "weight")
lines(gsize.seq, mu2.mean)
shade(mu2.HPDI, gsize.seq)
shade(weight2.PI, gsize.seq)


