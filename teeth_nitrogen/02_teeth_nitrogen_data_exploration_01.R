# setwd("//media//kswada//MyFiles//R//teeth_nitrogen")
setwd("//media//kswada//MyFiles//R//Variance_and_correlation_structure_model//teeth_nitrogen")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TeethNitrogen
# ------------------------------------------------------------------------------


TN <- read.table(file = "TeethNitrogen.txt", header = TRUE)


str(TN)


dim(TN)


car::some(TN)



# ------------------------------------------------------------------------------
# Data exploration:  data distribution
# ------------------------------------------------------------------------------

summary(TN)



# ----------
psych::describe(TN)


Hmisc::describe(TN)



# ----------
TN %>% filter(Tooth == "Moby") %>% xtabs(~ Age, data = .)


# -->
# "Moby" has 42 samples and age is from 3 to 44


# The Moby is 194 to 235th samples
which(TN$Tooth == "Moby")
