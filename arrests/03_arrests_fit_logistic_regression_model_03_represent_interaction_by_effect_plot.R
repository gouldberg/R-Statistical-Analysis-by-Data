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
# colour x age interaction
# ------------------------------------------------------------------------------
# Of course, one should by very wary of interpreting main effects when there are important interactions, 
# and the story turned out to be far more nuanced than was reported in the newspaper.

# The effect of age was in opposite directions for blacks and whites:
# Young blacks were indeed treated more severely than young whites
# However, for older people, blacks were treated less harshly than whites, controlling for all other predictors.

plot(effects::Effect(c("colour", "age"), arrests.mod),
     lwd = 3, multiline = TRUE, ci.style = "bands",  xlab = "Age", cex = 1.25, ylab = "Probability(released)",
     key.args = list(x = .05, y = .99, cex = 1.2, columns = 1))



# ------------------------------------------------------------------------------
# colour x year interaction
# ------------------------------------------------------------------------------
# Up to the year 2000 there was strong evidence for differential treatment on these charges, again controlling for other predictors.
# There was also evidence to support the claim by the police than in the year 2001 they began traiing of officers to reduce racial effects in treatment.
plot(effects::Effect(c("colour", "year"), arrests.mod),
     lwd = 3, multiline = TRUE, xlab = list("Year", cex = 1.25), ylab = list("Probability(released)", cex = 1.25),
     key.args = list(x = .7, y = .99, cex = 1.2, columns = 1))
