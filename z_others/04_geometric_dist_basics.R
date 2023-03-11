setwd("//media//kswada//MyFiles//R//visualization")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Geometric Distribution
#
#   - The geometric disribution is the special case of the negative binomial distribution when n (number of successes) = 1
#   - We observe a series of independent trials and count the number of non-events (failures) preceeding the first successful event.
#     The probability that there will be k failures before the first sucess is: Geom(p) = p(k) = p(1 - p)^k
#   - central moments: Mean(X) = 1/p, Var(X) = (1 - p)/p^2,  Skew(X) = (2 - p) / sqrt(1 - p)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# plot geometric distribution in multi-panel display:  lattice::xyplot()
# ------------------------------------------------------------------------------
# n:  number of successful events = 1
# p:  probability of success at each trial
# k:  number of non-events (failures)
n <- 1
p <- c(0.2, 0.4, 0.6, 0.8)
k <- 0:10
( mu <- n * (1 - p) / p )


# plot geometric distribution
XG <- expand.grid(k = k, n = n, p = p)
g_df <- data.frame(XG, prob = dnbinom(XG$k, XG$n, XG$p))
g_df$n <- factor(g_df$n)
g_df$p <- factor(g_df$p)

str(g_df)


lattice::xyplot(prob ~ k | p, data = g_df,
       xlab = list("Number of Failures (k)", cex = 1.25), ylab = list("Probability", cex = 1.25), type = c("h", "p"), pch = 16, lwd = 2)



# ----------
# Calculate theory-implied means
NP <- outer(n, p, function(n, p) n * (1 - p) / p)

dimnames(NP) <- list(n = n, p = p)
NP



# ------------------------------------------------------------------------------
# Plot geometric distribution in one display using matplot
# ------------------------------------------------------------------------------
# n:  number of successful events
# p:  probability of success at each trial
# k:  number of non-events (failures)
n <- c(1)
p <- c(0.2, 0.4, 0.6, 0.8)
k <- 0:10
( mu <- n * (1 - p) / p )


# uses outer() to create a matrix prob
( Prob <- outer(k, p, function(k,p) dnbinom(k, n, p)) )


# ----------
col <- palette()[2:5]

matplot(k, Prob, type = "o", pch = 15:18, col = col, lty = 1, xlab = "Number of Failures (k)", ylab = "Probability")
legend("topright", legend = c("0.2", "0.4", "0.6", "0.8"), pch = 15:18, lty = 1, col = col, title = "Pr(Failure)")



# ------------------------------------------------------------------------------
# plot geometric distribution using direct labels
# ------------------------------------------------------------------------------
mycol <- palette()[2:5]


# Note that the plot constructed by xyplot() is save as a ("trellis") object, plt.
# The function direct.label() messages this to add the labels directly to each curve.
# In the second argument above, "top.points" says to locate these at the maximum value on each curve.
plt <- xyplot(prob ~ k, data = g_df, groups = p,
              xlab = list("Number of Failures (k)", cex = 1.25), ylab = list("Probability", cex = 1.25),
              type = "b", pch = 15:18, lwd = 2, cex = 1.25, col = mycol, ylim = c(0, 1.0))

direct.label(plt, list("top.points", cex = 1.5, dl.trans(y = y + 0.1)))




