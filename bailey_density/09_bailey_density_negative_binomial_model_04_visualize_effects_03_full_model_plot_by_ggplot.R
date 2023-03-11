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
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

DF.fitp <- cbind(DF, DF_pred = predict(mod_obj, type = "response"))


head(DF.fitp)



# ----------
library(ggplot2)


graphics.off()

gg <- ggplot(DF.fitp, aes(x = MeanDepth, y = DF_pred)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg + facet_grid(~ fPeriod)

