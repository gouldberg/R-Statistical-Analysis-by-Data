setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Create a vector of the covariate
# that will be balanced by using propensity score methods
# ------------------------------------------------------------------------------

covariateNames <-  
  c("BYSTLNG2",#Sample member's English fluency
    "byplang",#Parent^s English fluency
    "byfcomp", #Family composition
    "bysibstr",# BY in-home sibling structure
    "bysibhom", # BY number of in-home siblings
    "bygnstat", # Generational status
    "bypared", #Parents' highest level of education
    "bygpared", #Highest reported level of education among parents^ parents
    "BYSES1QU", #Quartile coding of SES1 variable
    "bygrdrpt", #Number of grades repeated (K-10)
    "byhomlit", #BY home literacy resources
    "byriskfc", #Number of academic risk factors in 10th grade
    "bystexp", # How far in school student thinks will get-composite    
    "F1SEX", #F1 sex-composite    Composite    Weights and Composites for F1
    "F1RACE", #F1 student's race / ethnicity-composite	Composite	Weights and Composites for F1
    "F1HOMLNG", # 	F1 student's native language-composite	Composite	Weights and Composites for F1
    "BYS26", # 	High school program-student self-report	Questionnaire	BY Student Questionnaire
    "BYS27D", # 	Education is important to get a job later	Questionnaire	BY Student Questionnaire
    "BYS27I", # 	Parents expect success in school	Questionnaire	BY Student Questionnaire
    "BYS28", # 	How much likes school	Questionnaire	BY Student Questionnaire
    "BYS37", # 	Importance of good grades to student	Questionnaire	BY Student Questionnaire
    "BYS38C", # 	How often goes to class without homework done	Questionnaire	BY Student Questionnaire
    "BYS54A", # 	Importance of being successful in line work	Questionnaire	BY Student Questionnaire
    "BYS54O", # 	Importance of getting good education	Questionnaire	BY Student Questionnaire
    "BYS57", # 	Plans to continue education after high school	Questionnaire	BY Student Questionnaire
    "BYS58", # 	Type of school plans to attend	Questionnaire	BY Student Questionnaire
    "bysctrl", #public, private, other
    "byurban", #urbanicity
    "byregion", #geographic region of school
    "BY10FLP") #grade 10 percent free or reduced lunch



# ------------------------------------------------------------------------------
# Add missing values indicator variable
# ------------------------------------------------------------------------------

# check whether any dummy missing indicators are redundant because variables have missing values for the same cases
# dummy indicators for covariates with less than 5% missing data were not included to avoid model estimation problems.
# If two dummy missing value indicators have correlations above 0.8, only one of them is added to the propensity score model

missingCorrelations <- cor(missing.indicator[, propMissing > 0.05])


diag(missingCorrelations) <- 0


( maxCorrelations <- apply(missingCorrelations, 2, max) )


dummyNAnames <- names(maxCorrelations)[maxCorrelations < 0.8]


( maxCorrelationsHigh <- maxCorrelations[!duplicated(maxCorrelations)] )


( dummyNAnames <- c(dummyNAnames, names(maxCorrelationsHigh)[maxCorrelationsHigh >= 0.8]) )



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


