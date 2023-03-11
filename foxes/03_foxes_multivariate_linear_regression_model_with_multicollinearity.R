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
# correlation
# ------------------------------------------------------------------------------
pairs(d)

cor(d$area, d$groupsize)


# -->
# territory size and groupsize is strongly positively correlated


# ------------------------------------------------------------------------------
# Multivariate linear regression
# ------------------------------------------------------------------------------
# body weight as a linear function of both of territory size and group size
mod3 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba * area.s + bg * groupsize.s,
    a ~ dnorm(0, 10),
    ba ~ dnorm(0, 10),
    bg ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data =  d
)


precis(mod3, digits = 3, corr = TRUE)


# ----------
# The estimated coefs (absolute value) for ba and bg is larger than mod1 and mod2 (single variable regression)
par(mfrow=c(1,1))
plot(coeftab(mod1, mod2, mod3))



# ------------------------------------------------------------------------------
# plot raw data and counterfactual plot, holding other variable constant at its mean
# ------------------------------------------------------------------------------
area.seq <- seq(from=-3, to = 3, by = 0.1)

mu1 <- link(mod3, data = data.frame(area.s = area.seq, groupsize.s = mean(d$groupsize.s)))
sim.weight1 <- sim(mod3, data = list(area.s = area.seq, groupsize.s = mean(d$groupsize.s)), n = 1e3)
mu1.mean <- apply(mu1, 2, mean)
mu1.HPDI <- apply(mu1, 2, HPDI, prob = 0.89)
weight1.PI <- apply(sim.weight1, 2, PI, prob = 0.89)


gsize.seq <- seq(from=-3, to = 3, by = 0.1)

mu2 <- link(mod3, data = data.frame(groupsize.s = gsize.seq, area.s = mean(d$area.s)))
sim.weight2 <- sim(mod3, data = list(groupsize.s = gsize.seq, area.s = mean(d$area.s)), n = 1e3)
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


