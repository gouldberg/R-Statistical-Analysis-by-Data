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
# regression with taking into account measurement error both on outcome and predictors
# ------------------------------------------------------------------------------

dlist <- list(
  div_obs = d$Divorce,
  div_sd = d$Divorce.SE,
  mar_obs = d$Marriage,
  mar_sd = d$Marriage.SE,
  A = d$MedianAgeMarriage
)


# WAIC = FALSE: the default code in WAIC will not compute the likelihood correctly, by integrating over the uncertainty in each div_est distribution
# provided a start list for the div_est values, telling map2stan how many parameters it needs.
# It won't be sensitive to the exact starting values, but it makes sense to start each at the observed value for each State.
# default adapt_delta = 0.8, but providing 0.95, meaning that Stan will work harder during warmup and potentially sample more efficiently.

mod6 <- map2stan(
  alist(
    div_obs ~ dnorm(div_est, div_sd),
    mar_obs ~ dnorm(mar_est, mar_sd),
    div_est ~ dnorm(mu, sigma),
    mu <- a + bA * A + bR * mar_est[i],
    a ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 2.5)
  ), data = dlist,
  start = list(div_est = dlist$div_obs, mar_est = dlist$mar_obs),
  WAIC = FALSE, iter = 5000, warmup = 1000, chains = 2, cores = 10,
  control = list(adapt_delta = 0.95)
)



precis(mod6, digits = 3, depth = 2)

coeftab(mod0, mod3, mod4, mod5, mod6)



# -->
# Notice that the coefficients for age at marriage and marriage rate are essentially unchanged from the previous model.
# So adding error on the predictor did not change the major inference.
# But it did provide updated estimates of marriage rate itself.



# ------------------------------------------------------------------------------
# plot
#  1. Marriage rate estimated - observed  vs. Marriage rate standard error
#  2. divorced rate (posterior)  vs. Marriage rate (posterior)
# ------------------------------------------------------------------------------
div_obs <- as.vector(d$Divorce)
div_est <- as.vector(coef(mod6)[1:50])

mar_obs <- as.vector(d$Marriage)
mar_est <- as.vector(coef(mod6)[51:100])

y <- mar_est - mar_obs
dat <- data.frame(mar_obs_se = mar_obs_se, y = y)


dat2 <- data.frame(mar_obs = mar_obs, mar_est = mar_est, div_obs = div_obs, div_est = div_est)

head(dat2)



# ----------
par(mfrow=c(1,2))

plot(dat, xlab = "Marriage rate observed standard error", ylab = "Marriage rate estimated - divorce observed")
abline(h = 0, lty = 2)

plot(x = dat2$mar_obs, y = dat2$div_obs, ylim = c(4, 15), xlim = c(12, 33), xlab = "Marriage rate (posterior)", ylab = "Divorce rate (posterior)", col = "blue", pch = 16)
par(new=T)
plot(x = dat2$mar_est, y = dat2$div_est, ylim = c(4, 15), xlim = c(12, 33), xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = "black")

for(i in 1:nrow(dat2)){
  segments(x0 = dat2$mar_obs[i], y0 = dat2$div_obs[i], x1 = dat2$mar_est[i], y1 = dat2$div_est[i])
}


# -->
# plot1:
#  - shrinkage for the predictor variable marriage rate. Notice that shrinkage is not balanced,
#    but rather that the model believes the observed values tended to be overestimates.

# plot2:
#  - Shrinkage of both divorce rate and marriage rate.
#    Solid points are the observed values, open points are posterior means.
#    Lines connect pairs of points for the same State

# Also note that since there is not much association between divorce and marriage rates, there is less movement of the marriage rate estimates.
# That is to say that there is not much information in divorce rate to help us improve estimates of marriage rate.
