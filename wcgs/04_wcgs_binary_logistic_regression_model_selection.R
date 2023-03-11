setwd("//media//kswada//MyFiles//R//wcgs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wcgs
# ------------------------------------------------------------------------------

data("wcgs", package = "faraway")

str(wcgs)



# ------------------------------------------------------------------------------
# Model selection by step() with AIC based criterion
# ------------------------------------------------------------------------------

wcgs$bmi <- with(wcgs, 703 * wcgs$weight / (wcgs$height^2))

lmod <- glm(chd ~ age + height + weight + bmi + sdp + dbp + chol + dibep + cigs + arcus, family = binomial, wcgs)

lmodr <- step(lmod, trace = TRUE)


summary(lmodr)

faraway::sumary(lmodr)



# -->
# In this case, only two predictors are dropped from the full set: weight and diastolic blood pressure.



# ----------
drop1(glm(chd ~ dbp, family = binomial, wcgs), test = "Chi")


# -->
# We see that diastolic blood pressure is related to the change of heart disease ...
