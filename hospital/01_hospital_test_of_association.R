setwd("//media//kswada//MyFiles//R//hospital")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hospital
#  - 3 * 3 table relating the length of stay (in years) of 132 long-term schizophrenic patients in two London mental hospitals with
#    the frequency of visits of family and friends
# ------------------------------------------------------------------------------
data("Hospital", package = "vcd")

data <- Hospital

data

dimnames(data) <- list(VisitFrequency=c("Regular", "Less Than Monthly", "Never"), LengthOfStay = c("2-9", "10-19", "20+"))



# ------------------------------------------------------------------------------
# Sieve diagram
# ------------------------------------------------------------------------------
sieve(data, shade=TRUE)



# ------------------------------------------------------------------------------
# Cramer'sV =  0.365
# ------------------------------------------------------------------------------
assocstats(data)



# ------------------------------------------------------------------------------
# Test of association by chisq.test()
# ------------------------------------------------------------------------------
# Rejected no association hypothesis
chisq.test(data)



# ------------------------------------------------------------------------------
# Association plot
# ------------------------------------------------------------------------------
# There seems to be simple association between visit frequency and length of stay
assoc(~ VisitFrequency + LengthOfStay, data = data, shade = TRUE)



# ------------------------------------------------------------------------------
# CMHtest:  Both variables are ordianl, so CMH tests may be more powerful here.
# ------------------------------------------------------------------------------
CMHtest(data)


# --> Association seems more complex than expected.



# ------------------------------------------------------------------------------
# Visualizing two-way contingency table
# ------------------------------------------------------------------------------
plot(data, shade=TRUE)

tile(data, shade=TRUE)

spineplot(data)

mosaic(data, shade=TRUE)





