setwd("//media//kswada//MyFiles//R//milk")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  milk
# ------------------------------------------------------------------------------
data("milk", package = "rethinking")

d <- milk

dim(d)

str(d)



# ------------------------------------------------------------------------------
# rescale one of the explanatory variables  (DO NOT REMOVE MISSING VALUES)
# ------------------------------------------------------------------------------
# d <- milk[complete.cases(milk), ]
d$neocortex.prop <- d$neocortex.perc / 100
d$logmass <- log(d$mass)



# ------------------------------------------------------------------------------
# regression modeling WITH IMPUTING missing values
# ------------------------------------------------------------------------------

dlist <- list(
  kcal = d$kcal.per.g,
  neocortex = d$neocortex.prop,
  logmass = d$logmass
)


mod5 <- map2stan(
  alist(
    kcal ~ dnorm(mu, sigma),
    mu <- a + bN * neocortex + bM * logmass,
    neocortex ~ dnorm(nu, sigma_N),
    a ~ dnorm(0, 100),
    c(bN, bM) ~ dnorm(0, 10),
    nu ~ dnorm(0.5, 1),
    sigma_N ~ dcauchy(0, 1),
    sigma ~ dcauchy(0, 1)
  ),
  data = dlist,
  iter = 1e4, chains = 2, cores = 10
)


precis(mod5, digits = 3, depth = 2)




# ------------------------------------------------------------------------------
# regression modeling WITHOUT missing values
# ------------------------------------------------------------------------------

dcc <- d[complete.cases(d$neocortex.prop),]

dlist_cc <- list(
  kcal = dcc$kcal.per.g,
  neocortex = dcc$neocortex.prop,
  logmass = dcc$logmass
)


mod5.cc <- map2stan(
  alist(
    kcal ~ dnorm(mu, sigma),
    mu <- a + bN * neocortex + bM * logmass,
    a ~ dnorm(0, 100),
    c(bN, bM) ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1)
  ),
  data = dlist_cc,
  iter = 1e4, chains = 2, cores = 10
)


precis(mod5.cc, digits = 3, depth = 2)



# ----------
coeftab(mod5.cc, mod5)


# -->
# By including the incomplete cases, the posterior mean for neocortex has gone from 2.8 to 1.9, and the mean for body mass has diminished from -0.1 to -0.07
# So by using all the cases, the strength of the inferred relationships has diminished.



# ------------------------------------------------------------------------------
# plot
#  1. kcal per gram vs. neocortex proporion (raw data and imputed complete data)
#  2. neocortex proportion vs. log(mass)  --> how the association changed ?
# ------------------------------------------------------------------------------
div_est <- as.vector(coef(mod5)[1:50])
div_est_stdev <- precis(mod5, depth=2)@output$StdDev[1:50]
div_obs_se <- as.vector(d$Divorce.SE)

neo_obs <- as.vector(dlist_cc$neocortex)

y <- div_est - div_obs
dat <- data.frame(div_obs_se = div_obs_se, y = y)


post_5 <- extract.samples(mod5)

neo_seq <- seq(0.50, 0.85, by = 0.1)

mu <- link(mod5, data = data.frame(neocortex = neo_seq, logmass = mean(dlist$logmass)))
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





# ----------
# compute percentile interval of mean
np.seq <- -2:4
mu <- link(mod2, data = data.frame(log.mass = np.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)


plot(kcal.per.g ~ log.mass, data = dcc, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)




