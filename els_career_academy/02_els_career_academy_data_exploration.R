# setwd("//media//kswada//MyFiles//R//els_career_academy")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\els_career_academy")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ELS data example career academy
# ------------------------------------------------------------------------------

# data are "Spring 2002 Sophomores" (maybe)

load("Chapter2_ELS_data_example_career_academy.Rdata")


dim(ELS.data)


str(ELS.data)


# variable names starting with BY were measured in the base year (2002) of the Education Longtudinal Study (ELS)
# variable starting with F1 are composite variables created in 2004 but considered time invariant.


car::some(ELS.data)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(ELS.data)



# -->
# many NAs ...



# ----------
table(ELS.data$BYS33K)


prop.table(table(ELS.data$BYS33K))



# -->
# BYS33k ("treated") is only 7.9%



# ------------------------------------------------------------------------------
# data exploration:  statistical properties of student cross-sectional weights 2002
# ------------------------------------------------------------------------------

# not available for school cross-sectional weights "BYSCHWT" in this data


# bystuwt:
# Weights for cross-sectional analyses were created in the base year.
# BYSTUWT can be used for cross-cohort comparisons of students capable of completing
# the questionnaire (on a cross-cohort time-lag basis employing the sophomore classes of 1980 and 1990).
# Students who were (by virtue of disability or language barrier) unable to complete a questionnaire were nevertheless
# retained in the ELS:2002 sample (and contextual data and transcripts were gathered).

sum(ELS.data$bystuwt)


psych::describe(ELS.data$bystuwt)


length(unique(ELS.data$bystuwt))





