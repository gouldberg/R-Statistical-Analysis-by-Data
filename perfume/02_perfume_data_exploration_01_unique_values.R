setwd("//media//kswada//MyFiles//R//perfume")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  perfume
#  - sensory data collected at Agrocampus of 98 consumers conducting categorization task using 12 luxury perfumes
#  - The participants were asked to divide the perfumes into groups according to their sensory similarities, and then to attribute a description to each of the groups.
# ------------------------------------------------------------------------------

perfume <- read.table("perfume.csv", header = TRUE, sep = ";")

dim(perfume)

str(perfume)


car::some(perfume)



# ------------------------------------------------------------------------------
# summary or distributions
# ------------------------------------------------------------------------------

summary(t(perfume))

# summary(t(perfume), maxsum = Inf)



# -->
# Note that too many categorical values and rare words



# ----------
summary(perfume)



# ----------
Hmisc::describe(perfume)


table(perfume$J31)


perfume[,"J31"]



# -->
# For example, we can see that participant 31 (J31) separated the perfumes into 5 categories
# and that s/he categorised J'adore (eau de parfum) and J'adore (eau de toilette) in the same group.
