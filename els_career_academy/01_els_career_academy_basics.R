# setwd("//media//kswada//MyFiles//R//els_career_academy")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\els_career_academy")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ELS data example career academy
#   - data from the Education Longitudinal Study of 2002 (ELS)
#     More details about dataset can be found at http://nces.ed.gov/surveys/els2002/
#   - Career academies are programs within high schools that integrate academic preparation and workplace experiences through a career-focused curriculum.
#     Kemple and Willner (2008) reported on an experimental longitudinal study of the effect of career academies in nine urban high schools that followed
#     students from the start of high school until 8 years after their scheduled graduation.
#     Among their results, they found that participation in career academies increased average earnings of participants by $132 per month
#     during the first 4 years and $216 per month in the final 4 years, corresponding to an additional $2,088 in average earnings per year for program participants.
#   - For the career academy example, the population of interest comprises high school students.
#     The data set available from the ELS has 12,554 cases.
#     The treated and untreated groups are determines from the question "Have you ever been in any of the following kinds of courses or programs in high school?"
#     where option k is "Career Academy", from the base year student survey of the ELS
# ------------------------------------------------------------------------------

load("Chapter2_ELS_data_example_career_academy.Rdata")


dim(ELS.data)


str(ELS.data)


# variable names starting with BY were measured in the base year (2002) of the Education Longtudinal Study (ELS)
# variable starting with F1 are composite variables created in 2004 but considered time invariant.


car::some(ELS.data)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
