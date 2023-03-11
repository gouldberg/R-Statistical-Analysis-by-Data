setwd("//media//kswada//MyFiles//R//crime2")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crime2
#  - balanced panel of 46 cities, properly sorted, which is discused by Wooldridge (2016, Secion 13.3)
# ------------------------------------------------------------------------------

# library(foreign)
# crime2 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime2.dta")
# write.table(crime2, file = "crime2.txt", row.names = F, quote = F, sep = "\t")

# library(foreign)
# crime4 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime4.dta")
# write.table(crime4, file = "crime4.txt", row.names = F, quote = F, sep = "\t")


crime2 <- read.table("crime2.txt", header = T, stringsAsFactors = FALSE, sep = "\t")

crime4 <- read.table("crime4.txt", header = T, stringsAsFactors = FALSE, sep = "\t")


str(crime2)
str(crime4)

dim(crime2)
dim(crime4)

car::some(crime2)
car::some(crime4)



# ------------------------------------------------------------------------------
# Housman Taylor Estimator
#   - While the fixed effects model solves the problem of correlation between the error term and the regressors,
#     it does not allow time-invariant variables
#     Since the problem of the random effects model is endogeneity, one can use instrumental variables methods when time-invariant regressors must be in the model
#   - The Housman Taylor estimator uses instrumental variables in a random effects model
#     It assumes four categories of regressor:  time-varying exogenous, time-varying endogeneous, time-invariant exogenous, and time-invariant endogenous
#     The number of time-varying variables must be at least equal to the number of time-invariant ones.
# ------------------------------------------------------------------------------

