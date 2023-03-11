setwd("//media//kswada//MyFiles//R//youthdep")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  YouthDep
# ------------------------------------------------------------------------------

data("YouthDep", package = "MPsychoR")

str(YouthDep)

dim(YouthDep)



# ----------
# item15: "I am bad all the time"
# item21: "I never have fun at school"
# each of them scored on three categories.
cdisub <- YouthDep[,c("CDI15r", "CDI21r", "race")]



# ------------------------------------------------------------------------------
# Configural Frequency Analysis for higher-dimensional tables
#   - General method for contingency tables and aims to find so-called types and antitypes.
#     Cells that have significantly more observations than expected are identified as types, conversely, cells that have significantly less observations
#     than expected are declared as antitypes.
#   - Having higher-dimensional tables, dependency structures become more complex, and simple X^2-tests do not work anymore.
#     In this case we need to use log-linear models
# ------------------------------------------------------------------------------

# convert to aggregated table
cdisub

countdf <- as.data.frame(table(cdisub))

countdf



# ----------
library(cfa)


# analysis of configuration frequenceis (CFA, NOT Confirmatory Factor Analysis !!!)
# The table is now subject to a Congifural Frequency Analysis fit, and we detect types and antitypes by first usinga X^2 test
# and second a z-test which is less conservative.
fit.cdi <- cfa(countdf[, 1:3], countdf[, 4])

head(fit.cdi$table)



# ----------
# Types and antitypes by using X^2-test
fit.cdi$table[fit.cdi$table$sig.chisq == TRUE, 1:3]


# -->
# The X^2-test finds two types (observed frequencies are significantly larger than expected considering race)
# which suggest that White kids tend to score 0 on both items and Latino kids tend to score 2 on both items.



# ----------
# Types and antitypes by using z-test
fit.cdi$table[fit.cdi$table$sig.z == TRUE, 1:3]


# -->
# we obtain other types and antitypes
# The type/antitype patterns obtained using configural frequency analysis are in line with symmetric multiple CA map


