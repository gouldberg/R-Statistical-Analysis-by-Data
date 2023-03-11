setwd("//media//kswada//MyFiles//R//fat")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
#   - Measuring a body fat is not simple. Muscle and bone are denser than fat so an estimate of body density can be used to estimate the proportion
#     of fat in the body. Measuring someones's weight is easy but volume is more difficult.
#     One method requires submerging the body underwater in a tank and measuring the increase in the water level.
#     Most people would prefer not to be submerged underwater to get a measure of body fat so we would like to have an easier method.
#   - In order to develp such a method, researchers recorded age, weight, height, and 10 body circumference measurements for 252 men.
#     Each man's percentae of body fat was accurately estimated by an underwater weighting technique.
#   - brozek as the response estimated by Brozek's equation to estimate percent body fat from density)
# ------------------------------------------------------------------------------

data(fat, package="faraway")

str(fat)



# ----------
pairs(fat)


