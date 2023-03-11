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
# simulation parameters
# ------------------------------------------------------------------------------
# average log-odds of survival in the entire population of ponds
a <- 1.371

# the standard deviation of the distribution of log-odds of survival among ponds
sigma <- 1.458

# the number of ponds
nponds <- 60

# sample size in each ponds  --> add "5"
n_class <- c(5, 10, 25, 35)
n_each = 15

id_pond <- rep(1:4, each = n_each)
ni <- as.integer(rep(n_class, each = n_each))



# ------------------------------------------------------------------------------
# simulate true log-odds and survivors
# ------------------------------------------------------------------------------
# simulate all intercept values for each pond  (true log-odds)
a_pond <- rnorm(nponds, mean = a, sd = sigma)


dsim <- data.frame(pond = 1:nponds, ni = ni, true_a = a_pond, id_pond = id_pond)


# ----------
# simulate survivors
dsim$si <- rbinom(nponds, prob = logistic(dsim$true_a), size = dsim$ni)
dsim$p_true <- logistic(dsim$true_a)  # on probability scale



# ------------------------------------------------------------------------------
# compute estimates
# ------------------------------------------------------------------------------
# compute the no-pooling estimtates (on the probability scale)
dsim$p_nopool <- dsim$si / dsim$ni



# ----------
# compute the partial-pooling estimates
mod3 <- map2stan(
  alist(
    si ~ dbinom(ni, p),
    logit(p) <- a_pond[pond],
    a_pond[pond] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ), data = dsim, iter = 1e4, warmup = 1e3
)


precis(mod3, digits = 3, depth = 2)


estimated.a_pond <- as.numeric(coef(mod3)[1:nponds])
dsim$p_partpool <- logistic(estimated.a_pond)



# ------------------------------------------------------------------------------
# compute absolute error
# ------------------------------------------------------------------------------
dsim$nopool_error <- abs(dsim$p_nopool - dsim$p_true)
dsim$partpool_error <- abs(dsim$p_partpool - dsim$p_true)



# ------------------------------------------------------------------------------
# plot absolute error:  the higher the point, the worse the estimate
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(1:nponds, dsim$nopool_error, xlab = "pond", ylab = "absolute error", col = rangi2, pch = 16)
points(1:nponds, dsim$partpool_error)
abline(v = n_each + 0.5, lwd = 0.5)
abline(v = n_each * 2 + 0.5, lwd = 0.5)
abline(v = n_each * 3 + 0.5, lwd = 0.5)

nopool_error_ave <- as.vector(by(dsim$nopool_error, dsim$id_pond, mean))
partpool_error_ave <- as.vector(by(dsim$partpool_error, dsim$id_pond, mean))

for(i in 1:4) lines(x = c(n_each * (i-1) + 0.5, n_each * i + 0.5), y = c(nopool_error_ave[i], nopool_error_ave[i]), col = "blue", lty = 1)
for(i in 1:4) lines(x = c(n_each * (i-1) + 0.5, n_each * i + 0.5), y = c(partpool_error_ave[i], partpool_error_ave[i]), col = "black", lty = 2)


#-->
# blue:  no-pooling estimate and average absolute error
# black: part pooling estimate and average absolute error

# Both kinds of estimates are much more accurate for larger ponds, on the right side.
# Blue line is always above the black dashed line, indicating that the no-pool estimates shown by the blue points have higher average error in each group of ponds
# The distance between the blue line and the black dashed line grows as ponds get smaller
