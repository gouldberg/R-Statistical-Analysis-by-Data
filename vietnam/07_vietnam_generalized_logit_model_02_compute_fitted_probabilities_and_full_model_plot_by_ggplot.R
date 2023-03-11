setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)

str(Vietnam)


car::some(Vietnam)



# ----------
levels(Vietnam$response)


# choose "A" clearly as baseline category
Vietnam$response <- relevel(Vietnam$response, ref = "A")

levels(Vietnam$response)



# ------------------------------------------------------------------------------
# Fitted probabilities
# ------------------------------------------------------------------------------

predictors <- expand.grid(sex = c("Male", "Female"),
                          year = c(1, 2, 3, 4, 5))

( fit <- data.frame(predictors, predict(viet.multinom2, predictors, type = "probs")) )


fit2 <- reshape2::melt(fit,
                       measure.vars = c("A", "B", "C", "D"),
                       variable.name = "response",
                       value.name = "Probability")

levels(fit2$response) <- c("A", "B", "C", "D")

fit2



# ------------------------------------------------------------------------------
# Full-model plot by ggplot
# ------------------------------------------------------------------------------
library(ggplot2)


gg <- ggplot(fit2, aes(x = year, y = Probability)) + geom_line(aes(colour = response), size = 1.8) + geom_point(colour = "black", size = 1.2) + theme_bw() + facet_grid(~ sex)

gg





