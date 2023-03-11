## Eight Schools
## ----------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ベイズ推定\\eightschools\\")


# -------------------------------------------------------
# load data and
# -------------------------------------------------------
schools <- read.csv("eightschools.csv", header=TRUE)

J <- nrow(schools)
y <- schools$estimate
sigma <- schools$sd


# pooled estimate of y_bar
sum(y / sigma^2) / sum(1/sigma^2)

# posterior variance
1/sum(1/sigma^2)

# posterior sd
sqrt(1/sum(1/sigma^2))




# -------------------------------------------------------
# Run Stan
# -------------------------------------------------------
library(rstan)

iter <- 1000;  chains <- 4;
schools_fit <- stan(file="eightschools.stan", data=c("J", "y", "sigma"), iter=iter, chains=chains)
schools_fit <- stan(file="eightschools2.stan", data=c("J", "y", "sigma"), iter=iter, chains=chains)

print(schools_fit)
plot(schools_fit)

#
print(schools_fit, "theta[1]", probs=c(0.025, 0.975))


# re-run
iter2 <- 2000;  chains2 <- 4;
schools_fit1 <- stan(fit=schools_fit, data=c("J", "y", "sigma"), iter=iter2, chains=chains2)
print(schools_fit1)
plot(schools_fit1)

# examine and diagnose sampling problems
pairs(schools_fit1)

#
schools_sim <- extract(schools_fit1, permuted=TRUE)
hist(schools_sim$tau)

mean(schools_sim$theta[,1] > schools_sim$theta[,3])


# -------------------------------------------------------
# Posterior predictive simulations and graphs
# -------------------------------------------------------
# simulate posterior predictive replicated data in the original 8 schools
n_sims <- length(schools_sim$lp__)
y_rep <- array(NA, c(n_sims, J))
for(s in 1:n_sims) y_rep[s,] <- rnorm(J, schools_sim$theta[s,], sigma)


par(mfrow=c(5,4), mar=c(4,4,2,2))
hist(y, xlab="", main="y")
for(s in 1:19) hist(y_rep[s,], xlab="", main=paste("y_rep", s))


# difference between the best and second-best of the 8 coaching programs
test <- function(y){
  y_sort <- rev(sort(y))
  return(y_sort[1] - y_sort[2])
}

t_y <- test(y)
t_rep <- rep(NA, n_sims)
for(s in 1:n_sims) t_rep[s] <- test(y_rep[s,])

par(mfrow=c(1,1))
cat("T(y)=", round(t_y, 1), " and T(y_pre) has mean", round(mean(t_rep), 1), "and sd", round(sd(t_rep), 1), "\nPr (T(y_rep) > T(y)) = ", round(mean(t_rep > t_y), 2), "\n")
hist0 <- hist(t_rep, xlim=range(t_y, t_rep), xlab="T(y_rep)")
lines(rep(t_y, 2), c(0, 1e6))
text(t_y, .9*max(hist0$count), "T(y)", adj=0)


# -------------------------------------------------------
# Replicated data in new schools
# -------------------------------------------------------
theta_rep <- array(NA, c(n_sims, J))
y_rep <- array(NA, c(n_sims), J))
for(s in 1:n_sims){
  theta_rep[s,] <- rnorm(J, scools_sim$mu[s], schools_sim$tau[s])
  y_rep[s,] <- rnorm(J, theta_rep[s,], sigma)
}


# -------------------------------------------------------
# Direct simulation, Gibbs, and Metropolis
# -------------------------------------------------------
mu_hat <- function(tau, y, sigma){
  sum(y/(sigma^2 + tau^2)) / sum(1/(sigma^2 + tau^2))
}

V_mu <- function(tau, y, sigma){
  1/sum(1/(tau^2 + sigma^2))
}

n_grid <- 2000
tau_grid <- seq(0.01, 40, length=n_grid)
log_p_tau <- rep(NA, n_grid)
for(i in 1:n_grid){
  mu <- mu_hat(tau_grid[i], y, sigma)
  V <- V_mu(tau_grid[i], y, sigma)
  log_p_tau[i] <- 0.5*log(V) - 0.5*sum(log(sigma^2 + tau_grid[i]^2)) - 0.5*sum((y - mu)^2/(sigma^2 + tau_grid[i]^2))
}
