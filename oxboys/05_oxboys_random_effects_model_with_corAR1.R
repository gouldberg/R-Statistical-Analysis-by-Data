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



# ----------
lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)

m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = d,
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)


mod_obj <- m0



# ------------------------------------------------------------------------------
# model growth with random effects with corAR1
# ------------------------------------------------------------------------------

lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)


# Some degree of autocorrelation in the random component of height
# corAR1:  autoregressive process of order 1
m0_2 <- lme(height ~ age + I(age^2), data = d, 
          random = list(Subject = ~ age),
          correlation = corAR1(form = ~ age | Subject), control = lmc)



# -----------
summary(m0_2)



# -->
# Phi1 = 0.476
