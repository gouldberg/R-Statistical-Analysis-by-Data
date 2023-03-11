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
lmod <- aov(bright ~ operator, data = pulp)

mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = TRUE)

mod_obj <- mmod



# ------------------------------------------------------------------------------
# Predict fixed + random effect
# ------------------------------------------------------------------------------

# predicted values
fixef(mod_obj)
ranef(mod_obj)$operator

fixef(mod_obj) + ranef(mod_obj)$operator



# ----------
# We can also use the predict function
# We specify the random effects part of the prediction as ~0 meaning that this term is not present.
predict(mod_obj, re.form = ~ 0)



# Now we specify that the operator is 'a'
predict(mod_obj, newdata = data.frame(operator = "a"))



# ------------------------------------------------------------------------------
# fitted values
# ------------------------------------------------------------------------------

# level = 0:  we take the fitted values obtained by the population model
# level = 1: give the within-operator fitted values.

F0 <- fitted(mod_obj, level = 0)

F1 <- fitted(mod_obj, level = 1)

