setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ----------
modp <- glm(intensity ~ length + area * year, data = CodParasites, family = poisson(link = "log"))




# ------------------------------------------------------------------------------
# Overview of adequacy of a fitted model:  revidual-leverage graph
# ------------------------------------------------------------------------------
# We consider the residual-leverage graph (number 5) as being the most useful for asessing influential observations, 
# plotting residuals against leverages.

plot(modp)

plot(modp, which = 5)



# ------------------------------------------------------------------------------
# levearage in halfnorm plot
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

halfnorm(hatvalues(modp))



# ------------------------------------------------------------------------------
# Studentized (deletion) residuals vs. leverage, showing the value of Cook's distance by the area of the bubble symbol
# ------------------------------------------------------------------------------

influencePlot(modp)




# ------------------------------------------------------------------------------
# Extended version of plotting residuals against leverages
# ------------------------------------------------------------------------------
# An extended version is produced by the function car::influencePlot(),
# which additionally uses the size (area) of the plotting symbol to also show the value of Cook's D.

graphics.off()
op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
res <- car::influencePlot(modp, id.col = "blue", scale = 8, id.cex = 1.5, id.n = 3)
k <- length(coef(modp))
n <- nrow(na.omit(CodParasites))
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)



# ----------
# show data together with diagnostics for influential cases
idx <- which(rownames(na.omit(CodParasites)) %in% rownames(res))

cbind(na.omit(CodParasites)[idx, c("length", "area", "year")], res)



# -->
# some of the cases are particulary influential on the model coefficients overall: the largest Cook's D is 4.89 for case 1085.
# This observation:  year 2000 and tanafjord



# ------------------------------------------------------------------------------
# The collection of index plot
#  - plots various influence diagnostics against the observation numbers in the data
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(modp, vars = c("Cook", "studentized", "hat"), id.n = 4)



