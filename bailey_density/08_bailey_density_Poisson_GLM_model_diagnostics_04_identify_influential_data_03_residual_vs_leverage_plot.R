# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ----------
M1 <- glm(TotAbund ~ MeanDepth, data = DF, family = poisson(link = "log"))

M2 <- glm(TotAbund ~ MeanDepth * fPeriod,  data = DF,  family = poisson)

DF$LogSA <- log(DF$SweptArea)

M3 <- glm(TotAbund ~ MeanDepth * factor(Period) + offset(LogSA), data = DF,  family = poisson)



# ----------
mod_obj <- M3



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
n <- nrow(DF)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)



# ----------
# show data together with diagnostics for influential cases
idx <- which(rownames(DF) %in% rownames(res))

cbind(DF[idx, ], res)



# -->
# the largest Cook's D is more than 48 ....



# ------------------------------------------------------------------------------
# The collection of index plot
#  - plots various influence diagnostics against the observation numbers in the data
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(mod_obj, vars = c("Cook", "studentized", "hat"), id.n = 4)




# ------------------------------------------------------------------------------
# Cook's distance
#   - Cook's distance values are leave-one-observation-out measures of influence.
#     Any Cook's distance value larger than 1 may be considered incluential.
# ------------------------------------------------------------------------------

par(mfrow=c(1,1), mar = c(5,5,2,2))
plot(cooks.distance(mod_obj), type = "h", ylim = c(0,20), cex.lab = 1.5)
abline(h=0, lty = 2, lwd = 2)



# ----------
I <- 1:nrow(DF)
I[cooks.distance(mod_obj) > 1]


# -->
# We have 29 observations with Cook's distance larger than 1, to many to drop those ovservations. 
