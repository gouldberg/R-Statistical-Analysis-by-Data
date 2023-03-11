# setwd("//media//kswada//MyFiles//R//orange")
setwd("C:\\Users\\kouse\\Desktop\\R\\audiometric")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  audiometric
#   - see "A User's Guide to Principal Components" p.106
#   - the values are threshold measurements calibrated in units referred to as 'decibel loss' in comparison
#     to a reference standard for that particular instrument.
#     Observations are obtained, one ear at a time, for a number of frequencies
# ------------------------------------------------------------------------------

audiometric <- read.table("audiometric.txt", header=TRUE, sep="\t")

str(audiometric)

dim(audiometric)


audiometric



# ------------------------------------------------------------------------------
# Distribution for each variables
# ------------------------------------------------------------------------------

summary(audiometric[,2:9])


psych::describe(audiometric[,2:9])




# ------------------------------------------------------------------------------
# Correlation among variables
# ------------------------------------------------------------------------------

round(cor(audiometric[,2:9]), digits = 3)


psych::pairs.panels(audiometric[,2:9], method = "spearman", stars = TRUE)



