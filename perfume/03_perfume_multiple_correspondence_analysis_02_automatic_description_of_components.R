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
# Automatic description of the component
# ------------------------------------------------------------------------------

dimdesc(res.mca)$'Dim 1'$quali



# ----------
dimdesc(res.mca)$'Dim 1'$category



# ----------
# rounding
lapply(dimdesc(res.mca), lapply, round, 4)


lapply(dimdesc(res.mca), lapply, signif, 3)
