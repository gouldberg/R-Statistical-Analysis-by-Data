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
# Additionally, model nu and tau
# ------------------------------------------------------------------------------

g26 <- gamlss(height ~ treatment + pb(age) + re(random = ~1 | case), sigma.fo = ~pb(age), nu.fo = ~pb(age), family = "SHASH", data = Leukemia, trace = FALSE)

g26_2 <- gamlss(height ~ treatment + pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), family = "SHASH", data = Leukemia, trace = FALSE)


g27 <- gamlss(height ~ treatment + pb(age) + re(random = ~1 | case), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), family = "SHASH", data = Leukemia, trace = FALSE)

g27_2 <- gamlss(height ~ treatment + pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), family = "SHASH", data = Leukemia, trace = FALSE)



# -->
GAIC(g25, g26, g26_2, g27, g27_2, k = log(nrow(Leukemia)))



# ----------
# modeling nu and tau does not improve GAIC ..?


summary(g26_2)



# ------------------------------------------------------------------------------
# Check the best model g25
# ------------------------------------------------------------------------------

plot(g26_2)


wp(g25)
wp(g26_2)



# ----------
term.plot(g26_2, pages = 1, ask = FALSE)

term.plot(g26_2, what = "sigma", pages = 1, ask = FALSE)

term.plot(g26_2, what = "nu", pages = 1, ask = FALSE)


term.plot(g25, pages = 1, ask = FALSE)

term.plot(g25, what = "sigma", pages = 1, ask = FALSE)
