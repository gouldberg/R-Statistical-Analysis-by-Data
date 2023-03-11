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
# Normal QQ plots for the predicted random effects
# ------------------------------------------------------------------------------

qqnorm(mod_obj, ~ ranef(.))



# -->
# The plots should look like correlated random scatters around straight lines,
# if the normality assumptions for the random effects are reasonable.

