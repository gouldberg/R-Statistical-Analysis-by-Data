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
# Multiple Correspondence Analysis by FactoMineR with ventilation
# ------------------------------------------------------------------------------

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))

res.mca <- MCA(perfume, graph = TRUE)

summary(res.mca)



# ------------------------------------------------------------------------------
# Confidence ellipses around categories of a categorical variable
# (i.e., around the barycentre of the individuals carrying that category)
# to assess whether two categories are significantly different or not
# ------------------------------------------------------------------------------

plotellipses(res.mca, keepvar = c("J31", "J40", "J18", "J93"))



# ------------------------------------------------------------------------------
# Confidence ellipses around categories of a single categorical variable
# ------------------------------------------------------------------------------

# keepvar = 25: J31
plotellipses(res.mca, keepvar = 25, label = "none")

