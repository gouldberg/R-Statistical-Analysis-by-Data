setwd("//media//kswada//MyFiles//R//monastery")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  monastery
#   - This is example data for simulation.
#     Suppose you own a monastery that is in the business, like many monasteries before the invention of the printing press, of copying manuscripts.
#     You employ 1000 monks, and on any particular day about 1 of them finishes a manuscript.
#     Since the monks are working independently of one another, and manuscripts vary in length, some days produce 3 or more manuscripts,
#     and many days produce none.
#   - The data available to you about the rate at which manuscripts are completed is totaled up each day.
# ------------------------------------------------------------------------------

# Suppose the true rate is lambda = 1.5 manuscripts per day. We can simulate a month of daily counts
num_days <- 30
y <- rpois(num_days, 1.5)


# Also suppose that your monastery is turning a tidy profit, so you are considering purchasing another monastery.
# Before purchasing, you'd like to know how productive the new monastery might be.
# Unfortunately, the current owners do not keep daily record, so a head-to-head comparison of the daily totals is not possible.
# Instead, the owners keep weekly totals.
# Suppose the daily rate at the new monastery is actually lambda = 0.5 manuscript per day.
# To simulate data on a weekly basis, we just multiply this average by 7
num_weeks <- 4
y_new <- rpois(num_weeks, 0.5 * 7)


plot(y)
plot(y_new)



# ----------
# y: observed counts
# days: the number of days each count was totaled over
# new monastery is indicated
y_all <- c(y, y_new)
exposure <- c(rep(1, 30), rep(7, 4))
monastery <- c(rep(0, 30), rep(1, 4))

d <- data.frame(y = y_all, days = exposure, monastery = monastery)
d



# ------------------------------------------------------------------------------
# fit poisson regression: rate model
# ------------------------------------------------------------------------------
# compute the offset

d$log_days <- log(d$days)

mod1 <- map(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- log_days + a + b * monastery,
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 1)
  ), data = d
)


precis(mod1, corr=TRUE, digits=3)



# ----------
# Compute the posterior distribution os lambda in each monastery, but without the offset now.
# We do not use the offset again, when computing predictions, because the parameters are alrready on the daily scale, for both monasteries.
post <- extract.samples(mod1)

lambda_old <- exp(post$a)
lambda_new <- exp(post$a + post$b)

precis(data.frame(lambda_old, lambda_new))


# -->
# The new monasgery produces about half as many manuscripts per day.
