setwd("//media//kswada//MyFiles//R//perfume")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  perfume
#  - sensory data collected at Agrocampus of 98 consumers conducting categorization task using 12 luxury perfumes
#  - The participants were asked to divide the perfumes into groups according to their sensory similarities, and then to attribute a description to each of the groups.
# ------------------------------------------------------------------------------

perfume <- read.table("perfume.csv", header = TRUE, sep = ";", row.names = 1)

dim(perfume)

str(perfume)


perfume[,1:10]



# ------------------------------------------------------------------------------
# Multiple correspondence analysi by mjca()
# ------------------------------------------------------------------------------

library(ca)

perf.mjca <- mjca(perfume, lambda = "Burt")


summary(perf.mjca)



# -->
# 67.6% of the total inertia is accounted for in 2 dimensions.



# ----------
par(mfrow = c(1,1))
plot(perf.mjca, arrows = c(TRUE, TRUE))



# -->
# Difficult to understand ....


