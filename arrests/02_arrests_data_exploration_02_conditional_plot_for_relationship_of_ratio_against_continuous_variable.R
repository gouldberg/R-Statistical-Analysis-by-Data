setwd("//media//kswada//MyFiles//R//arrests")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra", "carData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arrests
# ------------------------------------------------------------------------------
data("Arrests", package = "carData")

dim(Arrests)

str(Arrests)

car::some(Arrests)

data <- Arrests

# To allow for possible nonlinear effects of year, this variable was treated as a factor rather than as a (linear) numeric variable
data$year <- as.factor(data$year)



# ------------------------------------------------------------------------------
# conditional plot for relationship of ratio against continuous variable
# Released ~ age | colour
# ------------------------------------------------------------------------------

gg <- ggplot(Arrests, aes(age, as.numeric(released == "Yes"), color = colour)) + 
  ylab("Released") + theme_bw() +
  geom_point(position = position_jitter(height = 0.02, width = 0))



# The smoothed curves and confidence bands show the result of fitting separate linear logistic regressions
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x, alpha = 0.2, size = 2, aes(fill = colour))



# add splines::ns(x, 4)
gg + stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ splines::ns(x, 4), alpha = 0.2, size = 2, aes(fill = colour))


