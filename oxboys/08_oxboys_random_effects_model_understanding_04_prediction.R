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
# Predicttion
# ------------------------------------------------------------------------------

# predicted coefficients
fixef(mod_obj)

ranef(mod_obj)



# ----------
# We specify the random effects part of the prediction as ~0 meaning that this term is not present.
predict(mod_obj, re.form = ~ 0)

predict(mod_obj)



# ----------
# Now we specify that the operator is 'a'
predict(mod_obj, newdata = data.frame(Subject = "11", age = 0.5))




# ------------------------------------------------------------------------------
# Plot predicted values
# ------------------------------------------------------------------------------

# augPred():  predicted values are obtained at the specified values of primary.
# If object has a grouping structure, predicted values are obtained for each group.

# This is model predictions from m2 at the individual tree level, overlaid on individual Loblolly pine growth data.
# The panel titles are the value of the Seed tree identifier.

augPred(mod_obj)

plot(augPred(mod_obj))





