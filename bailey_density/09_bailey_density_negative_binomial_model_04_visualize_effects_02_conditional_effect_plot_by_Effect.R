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
mod_obj <- mod.nbin



# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots

plot(Effect(c("MeanDepth", "fPeriod"), mod_obj), 
     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(1,2,3,4,5), col = c("black", gray(0.2), gray(0.4), gray(0.6), gray(0.8))))



