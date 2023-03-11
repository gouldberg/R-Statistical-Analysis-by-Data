setwd("//media//kswada//MyFiles//R//monastery")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  monastery
#
#   - Now imagine that the monks take breaks on some days. On those days, no manuscripts are completed.
#     We want to consider that any zero in the data can arise from 2 processes:
#       (1) the monks spent the day as breaks (drinking)
#       (2) they worked that day but nevertheless failed to complete any manuscripts
# ------------------------------------------------------------------------------

# define parameters
prob_drink <- 0.2  # 20% of days
rate_work <- 1  # average 1 manuscript per day


# sample one year of production
N <- 365


# simulate days monks drink (taking breaks)
drink <- rbinom(N, 1, prob_drink)


# simulate manuscripts completed
y <- (1 - drink) * rpois(N, rate_work)



# ------------------------------------------------------------------------------
# Check the data
# ------------------------------------------------------------------------------

# stacked histogram

simplehist(y, xlab = "manuscripts completed", lwd = 4)

zeros_drink <- sum(drink)
zeros_work <- sum(y == 0 & drink == 0)
zeros_total <- sum(y == 0)

lines(c(0, 0), c(zeros_work, zeros_total), lwd = 4, col = rangi2)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

mod1 <- map(
  alist(
    y ~ dzipois(p, lambda),
    logit(p) <- ap,
    log(lambda) <- al,
    ap ~ dnorm(0, 1),
    al ~ dnorm(0, 10)
  ),
  data = list(y = y)
)


precis(mod1, digits = 3)



# ----------
# probability drink --> close to 0.20
logistic(-1.57)


# rate finish manuscripts, when not drinking  --> close to 1
exp(-0.012)

