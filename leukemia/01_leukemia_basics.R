setwd("//media//kswada//MyFiles//R//leukemia")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Leukemia
#  - The data set is based on a clinical trial for treatments for acute lymphoblastic leukemia (ALL),
#    conducted at Harvard University.
#    Obesity and short stature are common side effects of leukemia treatments on children, and it is desirable that
#    treatments minimize this type of side effect without effectiveness being compromised.
#    Six hundred and eighteen (618) children were studied between 1987 and 1995 and three different treatments were applied:
#    intracranial therapy without radiation, conventional intracranial radiation therapy and intracranial radiation therapy twice a day.
#    The children's heights were measured approximately every six months.
#
#  - Variables:
#        - case:  subject identifier
#        - treatment:  1, 2 or 3
#        - height:  in centimetres
#        - age:  in years
#
#  - Here we are analysing simulated data of 1988 observations on 197 girls who were diagnosed with ALL between two and nine years old.
#    The number of observations per child varies between 1 and 21.
# ------------------------------------------------------------------------------

data("Leukemia", package = "gamlss.data")


str(Leukemia)

car::some(Leukemia)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

# par.plot():  plot parallel plots for each individual in a repeated measurement study.
# it is based on the coplot() function of R.

par.plot(height ~ age, subject = case, data = Leukemia, lty = 1:10)



# -->
# The growth of girls, which appears to fatten off around the age of 15 years.

