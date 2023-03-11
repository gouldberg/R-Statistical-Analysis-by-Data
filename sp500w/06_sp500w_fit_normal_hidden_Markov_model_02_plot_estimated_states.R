setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)




# ------------------------------------------------------------------------------
# plot estimated states on time series
# ------------------------------------------------------------------------------

# switch labels 1 and 3
culer <- 4 - posterior(fm)[,1]


culer[culer == 3] <- 4



graphics.off()

layout(matrix(c(1,2,1,3), 2), heights = c(1, 0.75))

par(mar = c(2.5, 2.5, 0.5, 0.5), mgp = c(1.6, 0.6, 0))


# S&P 500 weekly returns with estimated regimes labeled as a number, 1, 2, or 3
# The minimum value of -20% during the financial crisis has been truncated to improve the graphics.

plot(y, main = "", ylab = "S&P500 Weekly Returns", type = "h", col = gray(0.7), ylim = c(-0.11, 0.11))

text(y, col = culer, labels = 4 - posterior(fm)[,1])



# ----------
# Sample ACF of the squared retunrs

acf(y ^ 2, xlim = c(0.02, 0.5), ylim = c(-0.09, 0.5), panel.first = grid(lty = 2))




# ----------
# Histogram of the data with the 3 estimated normal densities superimposed

hist(y, breaks = 25, prob = TRUE, main = "", border = gray(0.7))

culer <- c(1,2,4)

pi.hat <- colSums(posterior(fm)[-1,2:4]) / length(y)

for(i in 1:3){
  mu <- norms.mle[1,i]
  sig <- norms.mle[2,i]
  x <- seq(-0.15, 0.12, by = 0.001)
  lines(x, pi.hat[4-i] * dnorm(x, mean = mu, sd = sig), col = culer[i])
}



# -->
# Note that regime 2 appears to represent a somewhat large-in-magnitude negative return, and may be a lone dip,
# or the start of end of a highly volatile period.




# ----------
# note that 1 --> 3, 3 --> 1  (switched)
# 3 fitted normals: N(mu3 = 0.04, sigma3 = 0.14),  N(mu2 = -0.034, sigma2 = 0.009),  N(mu1 = -0.003, sigma1 = 0.044)

( norms.mle <- round(matrix(para.mle[10:15], 2, 3), 3) %*% permu )

