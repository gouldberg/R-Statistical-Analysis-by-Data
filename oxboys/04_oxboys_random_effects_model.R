setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# model growth with random effects of Subject
# ------------------------------------------------------------------------------

# This produces errors.
# m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3) + (1|Seed), data = d)


# ----------
# default setting method fails without some adjustment
# lme fits start by using the EM algorithm to get reasonble close to the optimal parameter estimates, and then switch to Newton's method,
# which converges more quickly.
# Note that niterEM should rarely be incread from its default 0 when calling gamm (for future reference)

library(nlme)

lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)


m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = d,
          random = list(Subject = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)


summary(m0)


