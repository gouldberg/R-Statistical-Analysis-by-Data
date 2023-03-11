setwd("//media//kswada//MyFiles//R//waffle_divorce")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Waffle Divorce
# ------------------------------------------------------------------------------
data("WaffleDivorce", package = "rethinking")

d <- WaffleDivorce

dim(d)

str(d)



# ------------------------------------------------------------------------------
# fitting mutivariate linear regression model
# ------------------------------------------------------------------------------
# standardize predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)

d$Marriage.s <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)

mod3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR * Marriage.s + bA * MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d)


precis(mod3, corr = TRUE)



# ----------
# MAP values and the percentile intervals
plot(precis(mod3))


# -->
# The posterior mean for marriage rate, bR, is now close to zero
# The posterior mean for age at marriage, ba, has actually gotten slightly farther from zero, but is essentially unchanged.
# Once we know median age at marriage for a State, there is little or nor additional predictive power in also knowing the rate of marriage in that State.



# ------------------------------------------------------------------------------
# Residuals
# ------------------------------------------------------------------------------
mod4 <- map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b * MedianAgeMarriage.s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d)



# compute expected value at MPA, for each State
mu <- coef(mod4)["a"] + coef(mod4)["b"] * d$MedianAgeMarriage.s


# compute residual for each State
m.resid <- d$Marriage.s - mu


plot(m.resid)



# ----------
# Residual marriage rate in each State, after accounting for the linear association with median age at marriage.
plot(Marriage.s ~ MedianAgeMarriage.s, data = d)

abline(mod4)

for(i in 1:length(m.resid)){
  x <- d$MedianAgeMarriage.s[i]
  y <- d$Marriage.s[i]
  lines(c(x, x), c(mu[i], y), lwd = 0.5, col = col.alpha("black", 0.7))
}



# ------------------------------------------------------------------------------
# Counterfactual plots
#   - Show the implied predictions for imaginary experiments in which the different predictor variables can be changed independently of one another.
#   - This plots are called "counterfactual", because they can be produced for any values of the predictor variables you like, even unobserved or
#     impossible combinations lke very high median age of marriage and very high marriage rate.
#     There are no States with this combination, but in a countrfactual plot, you can ask the model for a prediction for such a State.
#   - The simplect use of a counterfactual plot is to see how the predictions change as you change only one predictor at a time.
# ------------------------------------------------------------------------------
# prepare new counterfactual data:
# build a new list of data that describe the counterfactual cases we wish to simulate predictions for.
# the observed values for MedianAgeMarriage.s are not used. Instead we compute the average value and then use this average inside the linear model.
A.avg <- mean(d$MedianAgeMarriage.s)

R.seq <- seq(from = -3, to = 3, length.out = 30)

pred.data <- data.frame(
  Marriage.s = R.seq,
  MedianAgeMarriage.s = A.avg
)



# ----------
# compute counterfactual mean divorve (mu)
mu <- link(mod3, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)



# ----------
# simulate counterfactual divorce outcomes
R.sim <- sim(mod3, data = pred.data, n = 1e4)

R.PI <- apply(R.sim, 2, PI)



# ----------
# display predictions, hiding raw data with type = "n"
plot(Divorce ~ Marriage.s, data = d, type = "n")
mtext("MedianAgeMarriage.s = 0")
lines(R.seq, mu.mean)
shade(mu.PI, R.seq)
shade(R.PI, R.seq)



# ----------
R.avg <- mean(d$Marriage.s)
A.seq <- seq(from = -3, to=3.5, length.out = 30)
pred.data2 <- data.frame(
  Marriage.s = R.avg,
  MedianAgeMarriage.s = A.seq
)

mu <- link(mod3, data = pred.data2)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

A.sim <- sim(mod3, data = pred.data2, n = 1e4)
A.PI <- apply(A.sim, 2, PI)

plot(Divorce ~ MedianAgeMarriage.s, data = d, type = "n")
mtext("Marriage.s = 0")
lines(A.seq, mu.mean)
shade(mu.PI, A.seq)
shade(A.PI, A.seq)



# ------------------------------------------------------------------------------
# posterior prediction plots
# ------------------------------------------------------------------------------
# call link without specifying new data
# so it uses original data
mu <- link(mod3)

mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

divorce.sim <- sim(mod3, n = 1e4)
divorce.PI <- apply(divorce.sim, 2, PI)



# ----------
# Predicted divorce rate against observed, with 89% confidence intervals of the average prediction
# The dashed line shows perfect prediction
plot(mu.mean ~ d$Divorce, col = rangi2, ylim = range(mu.PI), xlab = "Observed divorce", ylab = "Predicted divorce")
abline(a = 0, b = 1, lty = 2)
for(i in 1:nrow(d)) lines(rep(d$Divorce[i], 2), c(mu.PI[1,i], mu.PI[2, i]), col = rangi2)

# The easiest way to label a few select points is to use identify
# after executing the line of code, R will wait for you to click near a point inthe active plot window. It'll then place a label near that point.
identify(x = d$Divorce, y = mu.mean, labels = d$Loc, cex = 0.8)



# ----------
# Average prediction error for each State, with 89% interval of the mean (black line) and 89% prediction interval (gray +)
divorce.resid <- d$Divorce - mu.mean
o <- order(divorce.resid)

dotchart(divorce.resid[o], labels = d$Loc[o], xlim = c(-6,5), cex = 0.6)
abline(v = 0, col = col.alpha("black", 0.2))
for(i in 1:nrow(d)){
  j <- o[i]
  lines(d$Divorce[j] - c(mu.PI[1,j], mu.PI[2,j]), rep(i,2))
  points(d$Divorce[j] - c(divorce.PI[1,j], divorce.PI[2,j]), rep(i,2), pch = 3, cex = 0.5, col = "gray")
}

