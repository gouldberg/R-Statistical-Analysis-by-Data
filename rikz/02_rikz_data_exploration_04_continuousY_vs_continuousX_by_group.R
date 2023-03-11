# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

gg <- ggplot(RIKZ, aes(NAP, Richness)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  labs(y = "Richness", x = "NAP")



gg + facet_wrap(~ as.factor(Exposure))

# -->
# Exposure = 8 is hieghest richness (only Beach = 2)



# ----------
gg + facet_wrap(~ as.factor(Beach))

xtabs(~ Beach + Exposure, data = RIKZ)


# -->
# Beach = 5 (Exposure = 10) is large decreasing slope



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by coplot
# ------------------------------------------------------------------------------

formula = Richness ~ NAP | as.factor(Exposure)

coplot(formula, data = RIKZ, ylab = "Richness", xlab = "NAP", las=1)



# ----------
formula = Richness ~ NAP | as.factor(Beach)

coplot(formula, data = RIKZ, ylab = "Richness", xlab = "NAP", las=1)




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

xyplot(Richness ~ NAP | as.factor(Exposure), data = RIKZ, type = c("p", "g", "smooth"))

xyplot(Richness ~ NAP | as.factor(Beach), data = RIKZ, type = c("p", "g", "smooth"))




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by scatterplot
# ------------------------------------------------------------------------------

formula <- Richness ~ NAP | as.factor(Exposure)

scatterplot(formula, data = RIKZ)



# ----------
formula <- Richness ~ NAP | as.factor(Beach)

scatterplot(formula, data = RIKZ)



