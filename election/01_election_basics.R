# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//election")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  election
#   - Survey data from the 2000 American National Election Study.
#     Two sets of six questions with four responses each, asking respondents’ opinions of how well various traits
#     (moral, caring, knowledgable, good leader, dishonest, intelligent) describe presidential candidates Al Gore and GeorgeW. Bush.
#     The responses are (1) Extremely well; (2) Quite well; (3) Not too well; (4) Not well at all.
#     Many respondents have varying numbers of missing values on these variables.
#   - The data set also includes potential covariates
#       - VOTE3: the respondent’s 2000 vote choice (when asked)
#       - AGE: the respondent’s age
#       - EDUC: the respondent’s level of education
#       - GENDER: the respon-dent’s gender
#       - PARTY: the respondent’s Democratic-Republican partisan identification.
#    - VOTE3 is coded as (1) Gore; (2) Bush; (3) Other.
#      EDUC is coded as (1) 8 grades or less; (2) 9-11 grades, no further schooling; (3) High school diplomaor equivalency; (4) More than 12 years of schooling, no higher degree; (5) Junior or communitycollege level degree; (6) BA level degrees, no advanced degree; (7) Advanced degree.
#      GENDER is coded as (1) Male; (2) Female.
#      PARTY is coded as (1) Strong Democrat; (2) Weak Democrat; (3) Independent-Democrat; (4) Independent-Independent; (5) Independent-Republican; (6) Weak Republican; (7) Strong Republican
#
#   - A data frame with 1785 observations with 17 survey variables. Of these 1311 individuals provided responses on all 12 candidate evaluations
# ------------------------------------------------------------------------------

library(poLCA)

data(election)

str(election)


# ----------
dat <- election



# ------------------------------------------------------------------------------
# count NA
# ------------------------------------------------------------------------------

count_na <- function(x) sum(is.na(x))

apply(dat, 2, FUN = count_na)


