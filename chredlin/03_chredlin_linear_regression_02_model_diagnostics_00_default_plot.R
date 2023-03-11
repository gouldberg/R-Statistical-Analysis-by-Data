setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ----------
linmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)

# linmod <- lm(involact ~ race + fire + theft + age + income, data = chredlin)




# ------------------------------------------------------------------------------
# default model diagnostics plot
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(linmod)



# -->
# diagonal streak in the residual-fitted plot is caused by the large number of zero response values in the data.
# Turning a blind eye to this feature, we see no particular problem.
# The Q-Q plot looks fine too.


