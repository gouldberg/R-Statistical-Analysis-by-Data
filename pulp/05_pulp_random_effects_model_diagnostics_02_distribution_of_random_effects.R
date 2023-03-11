setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ----------
mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = TRUE)


mod_obj <- mmod




# ------------------------------------------------------------------------------
# Model disgnostics:  Check the assumption of normality distributed random effects
# ------------------------------------------------------------------------------

qqnorm(ranef(mod_obj)$operator[[1]], main = "Operator effects")

qqline(ranef(mod_obj)$operator[[1]])




