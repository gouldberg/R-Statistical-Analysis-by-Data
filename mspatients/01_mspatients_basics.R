setwd("//media//kswada//MyFiles//R//mspatients")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Diagnosis of MSPatients
#  - Landis and Koch (1977) gave data on the diagnostic classification of multiple sclerosis (MS) patients by two neurologists,
#    one from Winnipeg and one from New Orleans.
#    There were two samples of patients, 149 from Winnipeg and 69 from New Orleans, and each neurologist classified all patients
#    into one of four digagnostic categories: (a) Certain MS, (b) Probable MS, (c) Possible MS, (d) Doubtful, unlikely, or definitely not MS.
# ------------------------------------------------------------------------------
data("MSPatients", package = "vcd")

data <- MSPatients

data


data[, , "Winnipeg"]

data[, , "New Orleans"]



# ----------
# The distribution of degree of severity of MS may differ between the two patient samples.
# As well, for a given sample, two neurologists may be more or less strict about the boundaries between the rating categories
sieve(data[, , "Winnipeg"], shade=TRUE)

sieve(data[, , "New Orleans"], shade=TRUE)



# ----------
apply(data, 3, sum)