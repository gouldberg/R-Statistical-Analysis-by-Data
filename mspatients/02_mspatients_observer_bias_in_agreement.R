setwd("//media//kswada//MyFiles//R//mspatients")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Diagnosis of MSPatients
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



# ------------------------------------------------------------------------------
# Diagnosis bias in agreement
#
#  - With an ordered scale, it may happen that one observer consistently tends to classify the objects into higher or lower categories
#    than the other, perhaps due to using stricter thresholds for the boundaries between adjacent categories.
#    This bias produces differences in the marginal totals, and decreases the maximum possible agreement.
#  - While special tests exist for marginal homogeneity, the observer agreement chart shows this directly by the relation of the dark squares to the diagonal line
#    When the marginal totals are the same, the squares fall along the diagonal.
#  - !!!! The measures of agreement, k and B, cannot ddetermine whether lack of agreement is due to such bias, but the agreement chart can detect this.
# ------------------------------------------------------------------------------
# Weighted agreement charts for both patient samples in this data.
# Departure of the middle rectangles from the diagonal indicates lack of marginal homogeneity.
cotabplot(data, cond = "Patients", panel = cotab_agreementplot, text_gp = gpar(fontsize = 18), xlab_rot = 20)



# -->
# It can be seen that, for both groups of patients, the rectangles for the two intermediate categories lie largely below the digonal line (represeinting equality).
# This indicates that the Winnipeg neurologist tends to classify patients into more severe diagnostic categories.
# The departure from the diagonal is greater for the Winnipeg patients, for whom the Winnipeg neurologist uses the two most sever diagnostic categories very often,
# as can also be seen from the marginal totals printed in this plot margins.


agr1 <- agreementplot(data[, , "Winnipeg"])
agr2 <- agreementplot(data[, , "New Orleans"])
rbind(Winnipeg = unlist(agr1), Neworleans = unlist(agr2)) [, 1:2]


# --> 
# Nevertheless, there is a reasonable amount of agreement if one-step disagreements are allowed.
# B measures for exact agreement are much lower.




