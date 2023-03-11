setwd("//media//kswada//MyFiles//R//visualization")

packages <- c("dplyr", "vcd", "MASS", "lattice", "directlabels", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Binomial distribution
#
#   -  The binomial distribution, Bin(n, p), arises as the distribution of the number k of events of interest that occur in independent trials
#      when the probability of the event on any on trial is the constant value p = Pr(event)
#   -  For example, if 15% of the population has red hair, the number of redheads in randomly sampled groups of n = 10 might follow a binomial distribution, Bin(10, 0.15)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# plot binomial distribution in multi-panel display:  lattice::xyplot()
# ------------------------------------------------------------------------------
# n:  number of independent trials
# p:  probability of success at each trial
# k:  number of successes
n <- c(8, 12, 16)
p <- round(c(1/3, 1/2, 2/3), 2)
k <- 0:12


XN <- expand.grid(k = k, n = n, p = p)
bin_df <- data.frame(XN, prob = dbinom(XN$k, XN$n, XN$p))
bin_df$n <- factor(bin_df$n)
bin_df$p <- factor(bin_df$p)

str(bin_df)

lattice::xyplot(prob ~ k | n + p, data = bin_df,
                xlab = list("Number of Successes (k)", cex = 1.25), ylab = list("Probability", cex = 1.25), type = c("h", "p"), pch = 16, lwd = 2)



# ------------------------------------------------------------------------------
# Plot binomial distribution in one display using matplot
# ------------------------------------------------------------------------------
# n:  number of independent trials
# p:  probability of success at each trial
# k:  number of successes
n <- c(12)
p <- c(1/6, 1/3, 1/2, 2/3)
k <- 0:12


# uses outer() to create a 13*4 matrix prob
( Prob <- outer(k, p, function(k,p) dbinom(k, n, p)) )


# ----------
col <- palette()[2:5]

matplot(k, Prob, type = "o", pch = 15:17, col = col, lty = 1, xlab = "Number of Successes (k)", ylab = "Probability")
legend("topright", legend = c("1/6", "1/3", "1/2", "2/3"), pch = 15:17, lty = 1, col = col, title = "Pr(Success)")



