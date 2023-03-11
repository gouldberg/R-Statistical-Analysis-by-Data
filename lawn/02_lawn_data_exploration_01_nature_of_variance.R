setwd("//media//kswada//MyFiles//R//lawn")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lawn
# ------------------------------------------------------------------------------

data("lawn", package = "faraway")

str(lawn)


car::some(lawn)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

xtabs(~ manufact + machine, data = lawn)


xtabs(~ machine + speed, data = lawn)



# ------------------------------------------------------------------------------
# data exploration:  nature of variation
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(lawn, aes(x = manufact, y = time, color = speed, shape = machine)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0.0), size = 4) + 
  scale_color_grey()


