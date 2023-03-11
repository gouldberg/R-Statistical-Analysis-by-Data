setwd("//media//kswada//MyFiles//R//rwdq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RWDQ (Work Design Questionnaire R Package Authors)
#   - dataset from Mair et al. (2015). The authors were interested in finding out why package developers contribute to R.
#     Among other things, they presented three subscales of the Work Design Questionnaire (WDQ) by Morgeson and Humphrey (2006), 
#     in order to explore whether certain work design characteristics had an influence on the participation in package developments.
#   - data frame with 1055 individuals and 18 items: job complexity (22-24), information processing (25-27), 
#     problem solving (28-31), variety of skills (32-35), specialization (36-39). Item wordings:
#   - We consider a single WDQ subscale only: "knowledge characteristics" which includes 18 dichotomous items related to job complexity, information processing,
#     problem solving, skill variety, and specialization.
#
#   - wdq_22:  The work on R packages requires that I only do one task or activity at a time.
#   - wdq_23:  The work on R packages comprises relatively uncomplicated tasks.
#   - wdq_24:  The work on R packages involves performing relatively simple tasks.
#   - wdq_25:  The work on R packages requires that I engage in a large amount of thinking.
#   - wdq_26:  The work on R packages requires me to keep track of more than one thing at a time.
#   - wdq_27:  The work on R packages requires me to analyze a lot of information
#   - wdq_28:  The work on R packages involves solving problems that have no obvious correct answer.
#   - wdq_29:  The work on R packages requires me to be creative.
#   - wdq_30:  The work on R packages often involves dealing with problems that I have not encountered before.
#   - wdq_31:  The work on R packages requires unique ideas or solutions to problems.
#   - wdq_32:  The work on R packages requires data analysis skills
#   - wdq_33:  The work on R packages requires programming skills.
#   - wdq_34:  The work on R packages requires technical skills regarding package building and documentation.
#   - wdq_35:  The work on R packages requires the use of a number of skills.
#   - wdq_36:  The work on R packages is highly specialized in terms of purpose, tasks, or activities.
#   - wdq_37:  The tools, procedures, materials, and so forth used to develop R packages are highly specialized in terms of purpose.
#   - wdq_38:  The work on R packages requires very specialized knowledge.
#   - wdq_39:  The work on R packages requires a depth of expertise.
# ------------------------------------------------------------------------------

data("RWDQ", package = "MPsychoR")

str(RWDQ)


car::some(RWDQ)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
