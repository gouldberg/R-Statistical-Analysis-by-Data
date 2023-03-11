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



# ------------------------------------------------------------------------------
# Data Exploration:  data distribution
# ------------------------------------------------------------------------------

summary(DF1)



# -->
# There are one missing value for location (Xkm and Ykm)



# ----------
psych::describe(DF1)



# ----------
Hmisc::describe(DF1)


