# setwd("//media//kswada//MyFiles//R//hawaii")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)



# ------------------------------------------------------------------------------
# Data exploration:  data distribution
# ------------------------------------------------------------------------------

summary(Hawaii)



# -->
# There are 2 and 3 records with NA


Hawaii[is.na(Hawaii$Stilt.Oahu),]

Hawaii[is.na(Hawaii$Moorhen.Kauai),]




# ----------
psych::describe(Hawaii)


Hmisc::describe(Hawaii)

