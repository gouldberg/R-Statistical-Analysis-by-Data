setwd("//media//kswada//MyFiles//R//death")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  death
#   - 
# ------------------------------------------------------------------------------

death <- read.table("death.csv", header = TRUE, row.names = 1, sep = ";")

str(death)

dim(death)

head(death)


colnames(death) <- c("0_1", "1_4", "5_14", "15_24", "25_34", "35_44", "45_54", "55_64", "65_74", "75_84", "85_94", "95+")



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

