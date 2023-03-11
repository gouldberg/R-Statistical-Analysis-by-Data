setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------
data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# Compare computation time: no parallel, multicore, snow
# ------------------------------------------------------------------------------

set.seed(123)


# ----------
# allocate the n = 610 observations to K = 10 cross-validation subsets
rand1 <- sample(10, 610, replace = TRUE)



( nc <- detectCores() )


# ----------
system.time(
  capture.output(
    gamlssCV(y ~ pb(x), sigma.formula = ~ pb(x), data = abdom, family = LO, rand = rand1, parallel = "no", ncpus = nc)
  )
)


system.time(
  capture.output(
    gamlssCV(y ~ pb(x), sigma.formula = ~ pb(x), data = abdom, family = LO, rand = rand1, parallel = "multicore", ncpus = nc)
  )
)


system.time(
  capture.output(
    gamlssCV(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = LO, rand = rand1, parallel = "snow", ncpus = nc)
  )
)


# -->
# "snow" is fastest.
