setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
#   - Agresti (2002) cites data from Ashfold and Sowden (1970) on the association between two pulmonary conditions,
#     breathlessness and wheeze, in a large sample of coal miners.
#     The miners are classified into age groups, and the question treated by Agresti is whether the association between these two symptom
#     is homogeneous over age.
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data



# ------------------------------------------------------------------------------
# data exploration:  structable table
# ------------------------------------------------------------------------------

# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


dim(dat2)



# ----------
library(vcd)


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# data exploration:  aperm
# ------------------------------------------------------------------------------

# wheeze * Age * Breathlessness
aperm(dat2, c(2,3,1))




# ------------------------------------------------------------------------------
# data exploration:  convert to data.frame (long)
# ------------------------------------------------------------------------------

( df <- data.frame(dat2) )


( df <- data.frame(structable(. ~ Age, data = dat2)) )



# ------------------------------------------------------------------------------
# data exploration:  convert back to multi-way table
# ------------------------------------------------------------------------------

xtabs(~ Breathlessness + Wheeze + Age, data = df)




# ------------------------------------------------------------------------------
# data exploration:  margin table
# ------------------------------------------------------------------------------

margin.table(dat2, margin = c(1,2))
