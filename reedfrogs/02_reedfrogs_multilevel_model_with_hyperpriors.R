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



# ------------------------------------------------------------------------------
# Varying intercepts model  (1 intercept for 1 tank regardless of density)
# ------------------------------------------------------------------------------
# make the tank cluster variable
d$tank <- 1:nrow(d)


# Note that this model does not work in map()
mod1 <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(0, 5)
  ),
  data = d
)


precis(mod1, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# Varying intercepts MULTILEVEL model with hyperpriors for each tank
# ------------------------------------------------------------------------------
mod2 <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ),
  data = d, iter = 4000, chains = 4
)


precis(mod2, digits = 3, depth = 2)



# ----------
compare(mod1, mod2)


# -->
# multilvel model has only 38 effective parameters. There are 12 fewer effective parameters than actual parameters, because the prior assigned
# to each intercept shrinks them all towards the mean alpha.
# In this case, the prior is reasonably strong. The mean of each a_tank is around 1.4 
# --> this is a reguralizing prior, now the amount of regularization has been learned from the data itself.

precis(mod2, digits = 3, depth = 2)
mean(precis(mod2, depth = 2)@output$Mean[1:48])
sqrt(var(precis(mod2, depth = 2)@output$Mean[1:48]))


# Notice that the multilevel model mod2 has fewer effective parameters than the ordinary fixed model mod1.
# This is despite the fact that the ordinary model has fewer actual parameters, only 48 instead of 50.
# Extra two prameters in the multilevel model allowd it to learn a more aggressive regularizing prior, to adaptively regularize.
# This resulted in a less flexible posterior and therefpre fewer effective parameters.



# ------------------------------------------------------------------------------
# Visualize shrinkage effect by each cluster:  empirical vs. estimated and global average
# ------------------------------------------------------------------------------
post <- extract.samples(mod2)

# compute median intercept for each tank also transform to probability with logistic
d$propsurv.est <- logistic(apply(post$a_tank, 2, median))


plot(d$propsurv, ylim = c(0, 1), pch = 16, xaxt = "n", xlab = "tank", ylab = "proportion survival", col = rangi2)
axis(1, at = c(1, 16, 32, 48), labels = c(1, 16, 32, 48))
points(d$propsurv.est)
abline(h = logistic(median(post$a)), lty = 2)
abline(v  = 16.5, lwd = 0.5)
abline(v = 32.5, lwd = 0.5)
text(8, 0, "small tanks")
text(16+8, 0, "medium tanks")
text(32+8, 0, "large tanks")


# -->
# filled blue points:  empirical proportions of survivors in each tadpole tank
# black circles: 48 per-tank estimates from the multilevel model
# dashed line: posteriror median from the multilevel model

# In every tank, the posterior median from the multilevel model is closer to the dashed line than the empirical proportion is. (shrinkage)
# Notice that the estimates from the smaller tanks have shrunk farther from the blue points.
# Varying intercepts for the smaller tanks, with smaller sample sizes, shrink more.
# Also notice that the farther a blue point is from the dashed line, the greater the distance between it and the corresponding multilevel estimate.
# Shrinkage is stronger, the further a tank's empirical proportion is from the glabal average alpha.

# All phenomena arise from a common cause:  pooling information across clusters (tanks) to improve estimates.



# ------------------------------------------------------------------------------
# inferred population distribution of survival
# ------------------------------------------------------------------------------
par(mfrow=c(1,2))

# 100 Gaussian distributions of the log-odds of survival, sampled from the posterior
plot(NULL, xlim = c(-3, 4), ylim = c(0, 0.35), xlab = "log-odds survive", ylab = "Density")
for(i in 1:100) curve(dnorm(x, post$a[i], post$sigma[i]), add = TRUE, col = col.alpha("black", 0.2))


# ----------
# Survival probabilities for 8000 new simulated tanks, averaging over the posterior distribution
# Notice that there is uncertainty about both the locaion, alpha, and scale, sigma, of the population distribution of log-odds of survival,
# All of this uncertainty is propageted into the simulated probabilities of survival.
sim_tanks <- rnorm(8000, post$a, post$sigma)
dens(logistic(sim_tanks), xlab = "probability survive")



