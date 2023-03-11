setwd("//media//kswada//MyFiles//R//howell1")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Howell1
#   - partial census data for the Dobe area !King San, compiled from interviews conducted by Nancy Howell in the late 1960s.
#   - For the non-anthropologists reading along, the !King San are the most famous foraging population of the 20th centry, largely because of detailed
#     quantitative studies by people like Howell.
#   - 544 individuals has a recorded height (centimeters), weight (kilograms), age (years), and "maleness" (0 indicating female and 1 indicating male).
# ------------------------------------------------------------------------------
data("Howell1", package = "rethinking")

d <- Howell1

dim(d)

str(d)


d2 <- d[d$age >= 18, ]



# ------------------------------------------------------------------------------
# grid approximation of the posterior distribution for height data
# ------------------------------------------------------------------------------
mu.list <- seq(from = 140, to = 160, length.out = 2000)

sigma.list <- seq(from = 4, to = 9, length.out = 2000)

post <- expand.grid(mu = mu.list, sigma = sigma.list)

post$LL <- sapply(1:nrow(post), function(i) sum(dnorm(d2$height, mean = post$mu[i], sd = post$sigma[i], log = TRUE)))

post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)

post$prob <- exp(post$prod - max(post$prod))



# ------------------------------------------------------------------------------
# inspect the posterior distribution
# ------------------------------------------------------------------------------
# simple contour plot
contour_xyz(post$mu, post$sigma, post$prob)



# ----------
# simple heat map
image_xyz(post$mu, post$sigma, post$prob)



# ------------------------------------------------------------------------------
# sampling from the posterior
# ------------------------------------------------------------------------------
sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)

sample.mu <- post$mu[sample.rows]

sample.sigma <- post$sigma[sample.rows]



# ----------
plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))



# ----------
# marginal posterior densities of mu and sigma
dens(sample.mu)

dens(sample.sigma)



# ----------
# highest posterior density interval
HPDI(sample.mu)

HPDI(sample.sigma)



# ------------------------------------------------------------------------------
# inspect marginal posterior density
# ------------------------------------------------------------------------------
# show a normal approximation with the same mean and variance
#  --> check a tail (long tail or not)
dens(sample.sigma, norm.comp = TRUE)



# ------------------------------------------------------------------------------
# The case of small sample (only 20)
# ------------------------------------------------------------------------------
d3 <- sample(d2$height, size = 20)
mu.list <- seq(from = 150, to = 160, length.out = 2000)
sigma.list <- seq(from = 4, to = 20, length.out = 2000)
post2 <- expand.grid(mu = mu.list, sigma = sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i) sum(dnorm(d3, mean = post2$mu[i], sd = post2$sigma[i], log = TRUE)))
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) + dunif(post2$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod - max(post2$prod))

sample2.rows <- sample(1:nrow(post2), size = 1e4, replace = TRUE, prob = post2$prob)
sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]

plot(sample2.mu, sample2.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))


# show a normal approximation with the same mean and variance
#  --> longer tail at the top of the cloud of points, long tail of uncertainty towards higher values
dens(sample2.sigma, norm.comp = TRUE)





