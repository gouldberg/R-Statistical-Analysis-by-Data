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
Vietnam$response <- ordered(Vietnam$response)



# ------------------------------------------------------------------------------
# Visualize results for proportional odds model
# Full-model plots
# ------------------------------------------------------------------------------

viet.fitp <- cbind(Vietnam, predict(viet.polr, type = "probs"))


head(viet.fitp)



# ----------
library(reshape2)


plotdat <- melt(viet.fitp, 
                id.vars = c("sex", "year", "response"),
                measure.vars = c("A", "B", "C", "D"),
                variable.name = "Res",
                value.name = "Probability")


head(plotdat)



# ----------
library(ggplot2)

# library(directlabels)

graphics.off()

#gg <- ggplot(plotdat, aes(x = year, y = Probability)) + theme_bw() + geom_line(size = 2.5, aes(color = Res)) + geom_point(colour = "black", size = 1.5) +
#  facet_grid(~ sex, labeller = function(x, y) sprintf("%s = %s", x, y))

gg <- ggplot(plotdat, aes(x = year, y = Probability)) + theme_bw() + geom_line(size = 2.5, aes(color = Res)) + geom_point(colour = "black", size = 1.5) +
  facet_grid(~ sex)


gg

# direct.label(gg)


