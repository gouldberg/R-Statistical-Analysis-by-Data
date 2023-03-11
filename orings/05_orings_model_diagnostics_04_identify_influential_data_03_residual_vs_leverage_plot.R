setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ----------
lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)

summary(lmod)



# ----------
mod_obj <- lmod



# ------------------------------------------------------------------------------
# Overview of adequacy of a fitted model:  revidual-leverage graph
# ------------------------------------------------------------------------------
# We consider the residual-leverage graph (number 5) as being the most useful for asessing influential observations, 
# plotting residuals against leverages.

plot(mod_obj)

plot(mod_obj, which = 5)



# ------------------------------------------------------------------------------
# levearage in halfnorm plot
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

halfnorm(hatvalues(mod_obj))



# ------------------------------------------------------------------------------
# Studentized (deletion) residuals vs. leverage, showing the value of Cook's distance by the area of the bubble symbol
# ------------------------------------------------------------------------------

influencePlot(mod_obj)



# ------------------------------------------------------------------------------
# Extended version of plotting residuals against leverages
# ------------------------------------------------------------------------------
# An extended version is produced by the function car::influencePlot(),
# which additionally uses the size (area) of the plotting symbol to also show the value of Cook's D.

op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
res <- car::influencePlot(mod_obj, id.col = "blue", scale = 8, id.cex = 1.5, id.n = 3)
k <- length(coef(mod_obj))
n <- nrow(orings)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)



# ----------
# show data together with diagnostics for influential cases
idx <- which(rownames(orings) %in% rownames(res))

cbind(orings[idx, c("damage", "temp")], res)



# -->
# the largest Cook's D is 1.98 for case 1
# This observation also has the largest hat value.



# ------------------------------------------------------------------------------
# The collection of index plot
#  - plots various influence diagnostics against the observation numbers in the data
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(mod_obj, vars = c("Cook", "studentized", "hat"), id.n = 4)



