setwd("//media//kswada//MyFiles//R//fish")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fish
# ------------------------------------------------------------------------------
data("Fish", package = "rethinking")

d <- Fish

dim(d)

str(d)



# ------------------------------------------------------------------------------
# fit poisson regression: rate model
# ------------------------------------------------------------------------------
# compute the offset

d$log_hours <- log(d$hours)

mod1 <- map(
  alist(
    fish_caught ~ dpois(lambda),
    log(lambda) <- a + log_hours + b_bait * livebait + b_camp * camper + b_pers * persons + b_child * child,
    a ~ dnorm(0, 10),
    c(b_bait, b_camp, b_pers, b_child) ~ dnorm(0, 1)
  ), data = d
)


precis(mod1, corr=TRUE, digits=3)


postcheck(mod1, window = 260)

