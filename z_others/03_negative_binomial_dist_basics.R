setwd("//media//kswada//MyFiles//R//visualization")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Negative Binomial Distribution
#
#   - The negative binomial distribution is a type of waiting-time distribution, but also arises in statistical applications
#     as a generalization of the Poisson distribution, allowing for overdispersion (variance > mean).
#   - One form of the negative binomial distribution (also called the Pascal distribution) arises when a series of independent Bernoulli trials
#     is observed with constant probability p of some event, and we ask how many non-events (failures), k, it takes to observe n successful events.
#
#   - The variance is greater than the mean, and the distribution is always positively skewed.
#
#   - Greendwood and Yule (1920) developed the negative binomial distribution as a model for accident pronesness or susceptibility of individuals
#     to repeated attacks of disease.  They assumed that for any individual, i, the number of accidents or disease occurrences has a Poisson distribution
#     with parameter lambda(i).  If individuals vary in proneness, so that the lambda(i) have a gamma distribution, the resulting distribution is the negative binomial.
#   - In this form, the negative binomial distribution is frequently used as an alternative to the Poisson distribution when the assumptions of the Poisson
#     (constant probability and independence) are not satisfied, or when the variance of the distribution is greater than the mean (overdispersion).
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# calling dbinom(k, n, p) and dbinom(k, n, mu)
# ------------------------------------------------------------------------------
# n:  number of successful events
# p:  probability of success at each trial
# k:  number of non-events (failures)
n <- 2:4
p <- 0.2
k <- 2
( mu <- n * (1 - p) / p )


# ----------
# probabilities can be calculated using dnbinom() using the call dbinom(k, n, p)
# or call dbinom(k, n, mu)
dnbinom(k, n, p)

dnbinom(k, n, mu=mu)


# -->
# Thus, for the distribution with k = 2 failures and n = 2:4 successes with probability p = 0.2
# the values n = 2:4 correspond to means mu = 8, 12, 16



# ------------------------------------------------------------------------------
# plot negative binomial distribution in multi-panel display:  lattice::xyplot()
# ------------------------------------------------------------------------------
# n:  number of successful events
# p:  probability of success at each trial
# k:  number of non-events (failures)
n <- c(2, 4, 6)
p <- c(0.2, 0.3, 0.4)
k <- 0:20
( mu <- n * (1 - p) / p )


# plot negative binomial distribution with 3*3 combinations of parameters
XN <- expand.grid(k = k, n = n, p = p)
nbin_df <- data.frame(XN, prob = dnbinom(XN$k, XN$n, XN$p))
nbin_df$n <- factor(nbin_df$n)
nbin_df$p <- factor(nbin_df$p)

str(nbin_df)


lattice::xyplot(prob ~ k | n + p, data = nbin_df,
       xlab = list("Number of Failures (k)", cex = 1.25), ylab = list("Probability", cex = 1.25), type = c("h", "p"), pch = 16, lwd = 2)



# ----------
# Calculate theory-implied means
NP <- outer(n, p, function(n, p) n * (1 - p) / p)

dimnames(NP) <- list(n = n, p = p)
NP



# ------------------------------------------------------------------------------
# Plot negative binomial distribution in one display using matplot
# ------------------------------------------------------------------------------
# n:  number of successful events
# p:  probability of success at each trial
# k:  number of non-events (failures)
n <- c(4)
p <- c(0.2, 0.3, 0.4)
k <- 0:25
( mu <- n * (1 - p) / p )


# uses outer() to create a matrix prob
( Prob <- outer(k, p, function(k,p) dnbinom(k, n, p)) )


# ----------
col <- palette()[2:4]

matplot(k, Prob, type = "o", pch = 15:17, col = col, lty = 1, xlab = "Number of Failures (k)", ylab = "Probability")
legend("topright", legend = c("0.2", "0.3", "0.4"), pch = 15:17, lty = 1, col = col, title = "Pr(Failure)")

