setwd("//media//kswada//MyFiles//R//breastfeeding")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  breastfeeding
#   - Data from the National Longitudinal Survey of Youth 1979 (NLSY79) and the NLSY79 Children and Youth.
#     This sample contains 1,209 child record, restricted to one child per mother, mothers who had at least one job in the fourth quarter of pregnancy,
#     and mothers who returned to work within 12 weeks of the birth of the child.
#   - The NLSY79 data used were restricted to years 1988 to 1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, and 2010 because responses about the 
#     treatment of interest were available for these years.
#     Because the NLSY79 was not designed to be representative of the population of working mothers that is the focus of this example,
#     the NLSY79 sampling weights will not be used in this demonstration.
#   - The outcome variable is the age of the child in weeks when breastfeeding ended.
#     The treatment indicator is whether the mother's job provided or subsidized child care and was obtained from the NLSY79.
#
#   - The health and cognitive benefits of breastfeeding on children are well documented, therefore, it is important to understand
#     the factors that lead mothers to initiate and maintain breastfeeding, including job characteristics, because job demands can conflict with
#     breastfeeding efforts.
#     Jacknowitz (2008) examined the effects of mothers having a job that provides or subsidizes child care on whether mothers initiated
#     breastfeeding and on whether they breastfed until the child was 6 months old. Using multiple regression models, she found that
#     mothers who worked for a company that offered child care were more likely to breastfeed to 6 months.
# ------------------------------------------------------------------------------

load(file="//media//kswada//MyFiles//references//PracticalPropensityScoreMethodsUsingR//Chapter5//Chapter5_data_breastfeeding_example.rData")


str(data)

glimpse(data)
