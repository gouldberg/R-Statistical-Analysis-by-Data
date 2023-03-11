setwd("//media//kswada//MyFiles//R//reedfrogs")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  reedfrogs
#   - Experiments exploring Reed frog tadpole mortality.
#   - outcome is "surv"
# ------------------------------------------------------------------------------
data("reedfrogs", package = "rethinking")

d <- reedfrogs

dim(d)

str(d)


# ----------
d$tank <- 1:nrow(d)



# ------------------------------------------------------------------------------
# varing intercept only, using Gaussian distribution
# ------------------------------------------------------------------------------
mod4_0 <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ),
  data = d, iter = 5000, warmup = 1000, chains = 4
)


# plot(mod4_0)

precis(mod4_0, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# varing intercept only, using Cauchy distribution
# ------------------------------------------------------------------------------
mod4_1 <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dcauchy(a, sigma),  # using HalfCauhy distribution
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ),
  data = d, iter = 5000, warmup = 1000, chains = 4
)


# plot(mod4_1)


precis(mod4_0, digits = 3, depth = 2)
precis(mod4_1, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
compare(mod4_0, mod4_1)


# ----------
par(mfrow=c(1,2))
pr_i   <- precis(mod4_0, depth = 2)
pr_i_cauchy  <- precis(mod4_1, depth = 2)

pred_idx <- d$pred.cat == 1


par(mfrow=c(1,2))

plot(y=d[pred_idx,]$propsurv, x=d[pred_idx,]$tank, ylim=c(0,1), pch=19, col="black")
lines(y=d[pred_idx,]$propsurv, x=d[pred_idx,]$tank, ylim=c(0,1), col="black")
abline(h=median(d$propsurv), col="black", lty=2)
abline(h=median(d[pred_idx,]$propsurv), col="orange", lty=2)
points(y=inv_logit(pr_i@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="red")
lines(y=inv_logit(pr_i@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="red")
points(y=inv_logit(pr_i_cauchy@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="blue")
lines(y=inv_logit(pr_i_cauchy@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="blue")
text(20, 0, "no predation")

plot(y=d[! pred_idx,]$propsurv, x=d[! pred_idx,]$tank, ylim=c(0,1), pch=19, col="black")
lines(y=d[! pred_idx,]$propsurv, x=d[! pred_idx,]$tank, ylim=c(0,1), col="black")
abline(h=median(d$propsurv), col="black", lty=2)
abline(h=median(d[! pred_idx,]$propsurv), col="orange", lty=2)
points(y=inv_logit(pr_i@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="red")
lines(y=inv_logit(pr_i@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="red")
points(y=inv_logit(pr_i_cauchy@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="blue")
lines(y=inv_logit(pr_i_cauchy@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="blue")
text(20, 0, "predation")


# -->
# Most of the times, a_tank with the half cauchy distribution shrinks more towards to overall median compared to a_tank with Gaussian distribution.
# But in terms of WAIC, model with Gaussian distribution is better.
# The model with the half cauchy distribution overfits more ?


# ----------
par(mfrow=c(1,2))
plot(pr_i)
plot(pr_i_cauchy)


# -->
# There are some tanks with very large uncertainty by the model with the half cauchy distribution.



