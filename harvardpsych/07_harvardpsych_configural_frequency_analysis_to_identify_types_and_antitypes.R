setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HarvardPsych
# ------------------------------------------------------------------------------

data("HarvardPsych", package = "MPsychoR")

str(HarvardPsych)

dim(HarvardPsych)


# researchers in rows, words in columns  (29 * 43)
head(HarvardPsych)



# ------------------------------------------------------------------------------
# Configural Frequency Analysis for 2-dimensional table
#   - General method for contingency tables and aims to find so-called types and antitypes.
#     Cells that have significantly more observations than expected are identified as types, conversely, cells that have significantly less observations
#     than expected are declared as antitypes.
# ------------------------------------------------------------------------------

# The list of Docs and Terms (Rows and Columns)
( configs <- expand.grid(dimnames(HarvardPsych)) )


( counts <- as.vector(HarvardPsych) )



# ----------
# analysis of configuration frequenceis (CFA, NOT Confirmatory Factor Analysis !!!)
library(cfa)

fit.cfa <- cfa(configs, counts, binom.test = TRUE, sorton = "n")

names(fit.cfa)


head(fit.cfa$table, 15)


fit.cfa$summary.stats

fit.cfa$levels



# ----------
# print top 10 types out of a total 95 types, as identified by the binomial test.
types <- fit.cfa$table[fit.cfa$table$sig.bin == TRUE, 1:3]

head(types, 10)



# -->
# The strongest type is Dan Schacter who's researching (biological aspects) on human memory
# The observed word count clearly exceeds the count as expected under independence.

# Note that there are no antitypes in this application since the frequency table is very sparse.
