# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\sass_tfs")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SASS and TFS
#   - Does the assigning of mentors of different areas to new teachers affect the probability that they will
#     continue in the teaching profession in the following year ?
#     The treatment has 3 conditoins
#       - (1) No mentor was assigned
#       - (2) a mentor was assigned who was in the same subject area as the teacher
#       - (3) a mentor was assigned who was in a different subject area from the teacher
#   - Data on types of mentoring and covariates were obtained from the 1999 - 2000 School and Staffing Survey (SASS).
#     The sample consists of teachers of public schools with less than 3 years of experience.
#     There are 5,770 teachers in the sample, from which 2,299 (40%) were not assigned a mentor,
#     2,559 (44%) were assigned a mentro from the same subject area,
#     and 912 (16%) were asssigned a mentor from a different subject ara.
#   - The outcome, which is a binary indicator of whether the teacher left the teaching profession,
#     was obtained from the 2000 - 2001 Teacher Follow-up Survey (TFS).
#   - The public use SASS data set is provided with missing values already imputed and with missing data indicators available
#     as variables.
#     For the cases with missing data in school or principal variables that had not already been imputed in the SASS data set,
#     single imputation was performed using the mice package
# ------------------------------------------------------------------------------


load("SASS_TFS_data_imputed.Rdata")

str(imputedData)

names(imputedData)
