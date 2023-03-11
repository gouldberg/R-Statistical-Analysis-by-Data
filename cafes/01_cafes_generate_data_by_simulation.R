setwd("//media//kswada//MyFiles//R//cafes")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cafes  --> we generate by simulation
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# define average properties of the cafes
#
#  mu(i) = a(i) + b(i) * A(i)
#    - mu(i):  wait time minutes at cafe
#    - A(i):  0/1 indicator for afternoon
# ------------------------------------------------------------------------------
# average morning wait time
a <- 3.5


# average difference afternoon wait time
b <- (-1)


# std dev in intercepts
sigma_a <- 1


# std dev in slopes
sigma_b <- 0.5


# correlation between intercepts and slopes
rho <- (-0.7)



# ------------------------------------------------------------------------------
# Construct covariance matrix for simulating 2-dimensional Gaussian distribution
# ------------------------------------------------------------------------------
# mu
Mu <- c(a, b)


# ---------
# Covariance matrix:  method1
cov_ab <- sigma_a * sigma_b * rho

sigmas <- c(sigma_a, sigma_b)

Sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol = 2)



# ---------
# Covariance matrix:  method2
Rho <- matrix(c(1, rho, rho, 1), nrow = 2)  # correlation matrix

sigmas <- c(sigma_a, sigma_b)  # standard deviations

Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)


Sigma



# ------------------------------------------------------------------------------
# Simulate population of cafes
# ------------------------------------------------------------------------------

N_cafes <- 20

library(MASS)

set.seed(10)

vary_effects <- mvrnorm(N_cafes, Mu, Sigma)


vary_effects


# ----------
a_cafe <- vary_effects[,1]

b_cafe <- vary_effects[,2]



# ----------
cor(a_cafe, b_cafe)


rho



# ----------
plot(a_cafe, b_cafe, col = rangi2, xlab = "intercepts(a_cafe)", ylab = "slopes(b_cafe)")

library(ellipse)

for(l in c(0.1, 0.3, 0.5, 0.8, 0.99)) lines(ellipse(Sigma, centre = Mu, level = l), col = col.alpha("black", 0.2))



# ------------------------------------------------------------------------------
# Simulate observations
# ------------------------------------------------------------------------------

N_visits <- 10

( afternoon <- rep(0:1, N_visits * N_cafes / 2) )

( cafe_id <- rep(1:N_cafes, each = N_visits) )


# ----------
mu <- a_cafe[cafe_id] + b_cafe[cafe_id] * afternoon

sigma <- 0.5  # std dev within cafes

wait <- rnorm(N_visits * N_cafes, mu, sigma)

( d <- data.frame(cafe = cafe_id, afternoon = afternoon, wait = wait) )



