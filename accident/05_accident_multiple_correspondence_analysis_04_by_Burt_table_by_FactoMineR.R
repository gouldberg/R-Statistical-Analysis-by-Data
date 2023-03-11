setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


# frequency form in dataframe to table
( acci.tab <- xtabs(Freq ~ age + result + mode + gender, data = Accident) )



# ----------
# convert table to case form
acci_c <- expand.dft(acci.tab)



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis by FactoMineR from Burt Table
#  - Correspondence analysis of this table is used to represent the categories.
#    As this table is symmetrical, the representation of the cloud of row profiles is identical to that of the cloud of column profiles.
#    (only one of the two representations is therefore retained)
# ------------------------------------------------------------------------------

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))

res.mca <- MCA(acci_c, method = "Burt", quali.sup = 2)

summary(res.mca)



# -->
# This representation is very similar to the representation of the categories as provided by MCA and demonstrates
# the collinearity of the principal components of the same rank.

# However, the iinertias associated with each component differ.
# When lambda_s is the inertia of rank s for the MCA, the inertia of component rank s for a CA of the Burt table will be lambda_s^2

# Here the percentages of inertia associated with the first two components of the MCA are worth 25.26% and 20.04%, respectively

