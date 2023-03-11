# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)



# ------------------------------------------------------------------------------
# Data Exploration:  distribution
# ------------------------------------------------------------------------------

summary(Squid)


psych::describe(Squid)



# -->
# Note that min of TEstisweight is almos zero.
# No missing values



# ----------
Hmisc::describe(Squid)


# -->
# Year 1989 has only 9 samples