# setwd("//media//kswada//MyFiles//R//waiting_children//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//waiting_children//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  waiting children
#  - number of children and number of waiting children at 47 prefectures in 2014
# ------------------------------------------------------------------------------

# number of prefectures (data length)
N <- 47


# number of waiting children
x <- c(64,0,139,408,53,0,171,227,66,0,658,889,8672,880,17,0,
       0,0,0,0,27,96,107,48,372,11,461,349,76,5,0,3,
       23,0,57,41,0,0,3,306,50,1,359,0,0,185,1721)


# number of children
S <- c(40383,26285,21664,18573,18588,22094,18068,48652,24397,28400,76031,
       58988,211883,33504,43249,20155,25899,26095,21901,52025,41731,33543,
       95996,41427,23121,27009,60241,42512,19206,16064,17659,21707,17114,
       26789,20652,16213,12745,19967,12711,49827,21297,23912,32543,15030,
       18571,24963,30959)



# ------------------------------------------------------------------------------
# Beta distribution parameter:  sample and after shrinkage
# ------------------------------------------------------------------------------


# total # of successes and failures
alpha0 <- sum(x)

beta0 <- sum(S) - alpha0



# ----------
# alpha_samp, beta_samp:  beta parameter of sample
# alpha_sh, beta_sh:  beta parameter after shrinking by total alpha, beta

data <- data.frame(alpha_smp = x, beta_smp = S - x) %>%
  mutate(mu_smp = alpha_smp / (alpha_smp + beta_smp),
         sigma_smp = sqrt(alpha_smp * beta_smp / {(alpha_smp + beta_smp)^2 * (alpha_smp + beta_smp + 1)}),
         alpha_sh = alpha_smp + alpha0,
         beta_sh = beta_smp + beta0,
         mu_sh = alpha_sh / (alpha_sh + beta_sh),
         sigma_sh = sqrt(alpha_sh * beta_sh / {(alpha_sh + beta_sh)^2 * (alpha_sh + beta_sh + 1)}))

head(data)



# ------------------------------------------------------------------------------
# Beta distribution
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))


# N <- 100
N <- 10


# alpha
# alpha <- c(1, 10, 20, 50, 60, 80, 90, 99)
alpha <- c(1, 2, 5, 6, 8, 9)
# alpha <- c(0.1, 0.2, 0.3, 0.4, 0.5)


# beta
beta <- N - alpha


# mu
mu <- alpha / (alpha + beta)


# sigma
sigma <- sqrt(alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1)))


plot(0, 1, xlim=c(0.001, 0.999), ylim=c(0, N/1.1), ylab="", xlab="x", cex.axis=1.6, cex.lab=1.6, type = "n")

for(i in 1:length(mu)){
  par(new = T)
  curve(dBE(x, mu = round(mu[i], 3), sigma = round(sigma[i], 3)),
        xlim=c(0.001, 0.999), ylim=c(0, N/1.1), lty = i, lwd = 2, ylab="", xlab="x", col = i, cex.axis=1.6, cex.lab=1.6)
}



# ------------------------------------------------------------------------------
# Beta distribution from sample data
# ------------------------------------------------------------------------------


obj <- which(data$alpha_smp > 0)

samp <- sample(obj, size = 5, replace = FALSE)



graphics.off()

par(mfrow = c(1,1))


N <- data$alpha_smp[samp] + data$beta_smp[samp]


# alpha
alpha <- data$alpha_smp[samp]


# beta
beta <- data$beta_smp[samp]


# mu
mu <- alpha / (alpha + beta)


# sigma
sigma <- sqrt(alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1)))


plot(0, 1, xlim=c(0.001, 0.999), ylim=c(0, 100), ylab="", xlab="x", cex.axis=1.6, cex.lab=1.6, type = "n")

for(i in 1:length(mu)){
  par(new = T)
  curve(dBE(x, mu = round(mu[i], 3), sigma = round(sigma[i], 3)),
        xlim=c(0.001, 0.999), ylim=c(0, 100), lty = i, lwd = 2, ylab="", xlab="x", col = i, cex.axis=1.6, cex.lab=1.6)
}


# -->
# mu and sigma is very small and produces errors ...

