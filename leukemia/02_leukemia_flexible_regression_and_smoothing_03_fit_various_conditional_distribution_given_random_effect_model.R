setwd("//media//kswada//MyFiles//R//leukemia")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Leukemia
# ------------------------------------------------------------------------------

data("Leukemia", package = "gamlss.data")


str(Leukemia)

car::some(Leukemia)



# ------------------------------------------------------------------------------
# Fitting a various conditional distribution given a random effect model for mu
# ------------------------------------------------------------------------------

g22 <- gamlss(height ~ treatment + pb(age) + re(random = ~1 | case), sigma.fo = ~ pb(age), family = "LO", data = Leukemia, trace = FALSE)


g23 <- gamlss(height ~ treatment + pb(age) + re(random = ~1 | case), sigma.fo = ~pb(age), family = "BCPE", data = Leukemia, trace = FALSE)


g24 <- gamlss(height ~ treatment + pb(age) + re(random = ~1 | case), sigma.fo = ~pb(age), family = "ST3", data = Leukemia, trace = FALSE)


g25 <- gamlss(height ~ treatment + pb(age) + re(random = ~1 | case), sigma.fo = ~pb(age), family = "SHASH", data = Leukemia, trace = FALSE)



# -->
GAIC(g2, g21, g22, g23, g24, g25, k = log(nrow(Leukemia)))



# -->
# family = "SHASH" is the best in terms of GAIC



# ------------------------------------------------------------------------------
# Check the best model g25
# ------------------------------------------------------------------------------

plot(g25)


wp(g21)
wp(g25)



# ----------
term.plot(g21, pages = 1, ask = FALSE)

term.plot(g21, what = "sigma", pages = 1, ask = FALSE)


term.plot(g25, pages = 1, ask = FALSE)

term.plot(g25, what = "sigma", pages = 1, ask = FALSE)
