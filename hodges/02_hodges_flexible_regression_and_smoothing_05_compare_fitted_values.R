setwd("//media//kswada//MyFiles//R//hodges")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hodges
# ------------------------------------------------------------------------------

data("hodges", package = "gamlss.data")


str(hodges)

car::some(hodges)



# ------------------------------------------------------------------------------
# Compare fitted values
# ------------------------------------------------------------------------------

# Compare REML and ML
plot(prind ~ unclass(state), data = hodges, pch = ".", cex = 2, col = gray(0.6))

points(fitted(l1, level = 1) ~ unclass(state), data = hodges, pch = 5, col = "red")

points(fitted(l2, level = 1) ~ unclass(state), data = hodges, pch = 5, col = "blue")

lines(fitted(m0), lty = 2, col = "gray")



# ----------
# Compare marginal and conditional model
plot(prind ~ unclass(state), data = hodges, pch = ".", cex = 2, col = gray(0.6))

# marginal
points(fitted(l1, level = 0) ~ unclass(state), data = hodges, pch = 5, col = "red")

# conditional:  BLUP (best linear unbiased predictors) or EBP (empirical Bayes predictors)
points(fitted(l1, level = 1) ~ unclass(state), data = hodges, pch = 5, col = "blue")

lines(fitted(m0), lty = 2, col = "gray")



# ------------------------------------------------------------------------------
# Check shrinkage: m1 vs. m3
# ------------------------------------------------------------------------------

plot(prind ~ unclass(state), data = hodges, pch = ".", cex = 2, col = gray(0.6))

points(fitted(m1) ~ unclass(state), data = hodges, pch = 5, col = "red")

# shrink the level means towards the overall mean
points(fitted(m3) ~ unclass(state), data = hodges, pch = 5, col = "blue")

lines(fitted(m0), lty = 2, col = "gray")



# ------------------------------------------------------------------------------
# overall mean and fixed effect estimate
# ------------------------------------------------------------------------------

# overall mean
mean(hodges$prind)

fitted(m0)[1]


# fixed effect estimate
fitted(l1, level = 0)[1]

fitted(l2, level = 0)[1]

