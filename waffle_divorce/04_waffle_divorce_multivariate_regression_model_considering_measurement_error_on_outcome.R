setwd("//media//kswada//MyFiles//R//waffle_divorce")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Waffle Divorce
# ------------------------------------------------------------------------------
data("WaffleDivorce", package = "rethinking")

d <- WaffleDivorce

dim(d)

str(d)



# ------------------------------------------------------------------------------
# regression without taking into account measurement error
# ------------------------------------------------------------------------------
dlist <- list(
  div_obs = d$Divorce,
  R = d$Marriage,
  A = d$MedianAgeMarriage
)


mod0 <- map(
  alist(
    div_est ~ dnorm(mu, sigma),
    mu <- a + bA * A + bR * R,
    a ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bR ~ dnorm(0, 10),
    sigma ~ dunif(0, 100)  #dcauchy(0, 10)
  ), 
  data = dlist,
  control = list(maxiter=1e5)
)


precis(mod0, digits = 3, corr = TRUE)



# ------------------------------------------------------------------------------
# regression with taking into account measurement error on outcome
# ------------------------------------------------------------------------------

dlist <- list(
  div_obs = d$Divorce,
  div_sd = d$Divorce.SE,
  R = d$Marriage,
  A = d$MedianAgeMarriage
)


# WAIC = FALSE: the default code in WAIC will not compute the likelihood correctly, by integrating over the uncertainty in each div_est distribution
# provided a start list for the div_est values, telling map2stan how many parameters it needs.
# It won't be sensitive to the exact starting values, but it makes sense to start each at the observed value for each State.
# default adapt_delta = 0.8, but providing 0.95, meaning that Stan will work harder during warmup and potentially sample more efficiently.

mod5 <- map2stan(
  alist(
    div_obs ~ dnorm(div_est, div_sd),
    div_est ~ dnorm(mu, sigma),
    mu <- a + bA * A + bR * R,
    a ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 2.5)
  ), data = dlist,
  start = list(div_est = dlist$div_obs),
  WAIC = FALSE, iter = 5000, warmup = 1000, chains = 2, cores = 10,
  control = list(adapt_delta = 0.95)
)


precis(mod0, digits = 3, depth = 2)
precis(mod5, digits = 3, depth = 2)

coeftab(mod0, mod3, mod4, mod5)



# -->
# Notat the estimate for bA was about -1 (mod3), now it is almost half that, but still reliably negative
# So compared to the original regression that ignores measurement error, the association between divorce and age at marriage has been reduced.
# Ignoring measurement error tends to exaggerate associations between outcomes and predictors. But it might also mask association.



# ------------------------------------------------------------------------------
# plot
#  1. divorced estimated - observed  vs. divorce observed standard error
#  2. divorced rate (posterior)  vs. Median age marriage  (comparison of regression that ignores measurement error)
# ------------------------------------------------------------------------------
div_est <- as.vector(coef(mod5)[1:50])
div_est_stdev <- precis(mod5, depth=2)@output$StdDev[1:50]
div_obs_se <- as.vector(d$Divorce.SE)
div_obs <- as.vector(d$Divorce)
y <- div_est - div_obs
dat <- data.frame(div_obs_se = div_obs_se, y = y)


post_0 <- extract.samples(mod0)
post_5 <- extract.samples(mod5)

med_seq <- seq(20, 30, by = 0.1)

mu0 <- link(mod0, data = data.frame(A = med_seq, R = mean(d$Marriage, weight = d$Population)))
mu0.mean <- apply(mu0, 2, mean)
mu0.PI <- apply(mu0, 2, PI, prob = 0.97)

mu <- link(mod5, data = data.frame(A = med_seq, R = mean(d$Marriage, weight = d$Population)))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.97)

dat2 <- data.frame(A = d$MedianAgeMarriage, div_est = div_est, div_est_stdev = div_est_stdev)


# ----------
par(mfrow=c(1,2))

plot(dat, xlab = "Divorce observed standard error", ylab = "Divorce estimated - divorce observed")
abline(h = 0, lty = 2)

plot(dat2$A, dat2$div_est, ylim = c(4, 15), xlab = "Median age marriage", ylab = "Divorce rate (posterior)")
for(i in 1:nrow(dat2)){
  ci <- dat2$div_est[i] + c(-1, 1) * dat2$div_est_stdev[i]
  x <- dat2$A[i]
  lines(c(x,x), ci)
}
lines(med_seq, mu0.mean, col = "blue", lwd = 1, lty = 2)
shade(mu0.PI, med_seq, col = col.alpha("blue", 0.15))
lines(med_seq, mu.mean, col = "red", lwd = 2)
shade(mu.PI, med_seq, col = col.alpha("red", 0.15))


# -->
# plot1:
#  - shrinkage resulting from modeling the measurement error.
#  - The less error in the original measurement, the less shrinkage in the posterior estimate

# plot2:
#  - Comparison of regression that ignores measurement error with regression that incorporates measurement error
#  - THe points and line segments show the posterior means and standard deviations for each posterior divorce rate.
