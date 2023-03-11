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
library(nlme)

lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)


m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = d,
          random = list(Subject = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)


mod_obj <- m0



# ------------------------------------------------------------------------------
# Model understanding:  age effect
# ------------------------------------------------------------------------------

( rf1 <- ranef(mod_obj)$'age' )
( rf2 <- ranef(mod_obj)$'I(age^2)' )
( rf3 <- ranef(mod_obj)$'I(age^3)' )


summary(rf1)
summary(rf2)
summary(rf3)



# -->
# for 'age' effect, the difference between the best and the worst is about 6.6

