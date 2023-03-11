setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Create a vector of the covariate
# that will be balanced by using propensity score methods
# ------------------------------------------------------------------------------

# The covariates used here included most variables controlled for by Na and Gottfredson (2013)
# e.g., categories of total enrollment, percentage of students in special education, perrcentage of students who are eligible for free or reduced-price lunch,
# percentage of minority students, average daily attendance, crime level in the school's area, school locale, and grade level),
# as well as indicators of the security measures used in the school
# e.g., use of locks, uniforms, training of teachers, written plans.
# A total of 63 covariates were selected


# Get predictors of treatment assignment
# variable C0116 (metal detectors) was excluded because 97.4% of the schools 
# do not have students pass through metal detectors and only 1 of the untreated
# school have them pass through metal detectors

covariateNames <- names(SSOCS.data)[c(5:6,15:27,29,31,33,35:37,39:54,80:85,150:155,162:164,178,
                                      197:200, 417:422)]

covariateNames


# ------------------------------------------------------------------------------
# Add missing values indicator variable
# ------------------------------------------------------------------------------

# Following Stuart's (2010) recommendation for propsensity score estimation with single imputed data sets,
# dummy missing value indicators were added in the propensity score model.

# The imputation indicators provided with the SSOCS data set for each imputed value are categorical variables with three levels indicating
# whether the value was not imputed, imputed with exact match, or imputed with a relaxed criterion.
# These were converted into binary indicators (inputed or not imputed) and included in the propensity score model for variables with more than 5%
# of imputed values.
# If two variables had imputed values for similar sets of cases (i.e., the correlation between their dummy missing vaue indicators was above 0.8),
# only one of the indicators was included.
# This procedure produced 4 dummy missing indicators, and therefore the propensity score model has 67 predictors.

head(SSOCS.data[,c(252:414)])
names(SSOCS.data[,c(252:414)])


# convert missing value indicators into binary variables
for (var in 252:414) {
  SSOCS.data[,var] <- ifelse(SSOCS.data[,var] == 0, 0, 1) }



# ----------
# obtain proportions of missing data for variables of interest
missingIndicatorNames <- paste("I", covariateNames, sep="")

# remove composite variables
missingIndicatorNames <- missingIndicatorNames[-c(53:57)]

propMissing = apply(SSOCS.data[,missingIndicatorNames], 2, mean)

missingIndicatorNames <- missingIndicatorNames[propMissing > 0.05]

missingIndicatorNames


# ----------
# obtain imputation flags for variables of interest
# check whether any dummy missing indicators are redundant because variables have missing values for the same cases

missingCorrelations <- cor(SSOCS.data[,missingIndicatorNames])

diag(missingCorrelations) <- 0

maxCorrelations <- apply(missingCorrelations,2,max) 

dummyNAnames <- names(maxCorrelations)[maxCorrelations < 0.8]

maxCorrelationsHigh <- maxCorrelations[!duplicated(maxCorrelations)]

dummyNAnames <- c(dummyNAnames, names(maxCorrelationsHigh)[maxCorrelationsHigh >= 0.8])



# ----------
# add missing value indicators for variables with more than 5% of missing data to covariateNames
# merge covariate names with missing indicator names
( covariateNames <- c(covariateNames, dummyNAnames) )



# ------------------------------------------------------------------------------
# Create formula
# ------------------------------------------------------------------------------

# create formula based on covariate list this includes both individual level and school level covariates
psFormula <- paste(covariateNames, collapse="+")

psFormula <- formula(paste("treat~", psFormula, sep=""))

print(psFormula)


