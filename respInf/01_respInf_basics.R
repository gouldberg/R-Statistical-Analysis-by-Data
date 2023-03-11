setwd("//media//kswada//MyFiles//R//respInf")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  respInf
#  - Sommer et al. (1983) describe a cohort study of 275 Indonesian preschool children examined on up to six consecutive quarters
#    for the presence of respiratory infection.
#  - Variables:
#       - id:  a factor twith 275 levels identifying the individual children
#       - time:  the binary response variable identifying the presence of respiratory infection
#       - resp:  a vector of ones (not used further)
#       - age:  the ag in months (centred around 36)
#       - xero:  a dummy variable for the presence of xerophthalmia
#       - cosine:  a cosine term of the annual cycle
#       - sine:  a sine term of the annual cycle
#       - female:  a gender dummy variable, 1 = female, 0 = male
#       - height:  height for age as percent of the National Centre for Health Statistics standard, centred at 90%
#       - stunted:  a dummy variable whether below 85% in height for age
#       - time.1:  the times that the child hs been examined, 1 to 6
#       - age1:  the age of the child at the first time of examination
#       - season:  a variable taking the values 1,2,3,4 indicating the season
#       - time2:  the time in months
# ------------------------------------------------------------------------------
data("respInf", package = "gamlss.data")


str(respInf)

car::some(respInf)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------



