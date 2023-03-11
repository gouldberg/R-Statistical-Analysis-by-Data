setwd("//media//kswada//MyFiles//R//victims_of_crime")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Victims of Crime
#  - Variables
#       - reported:  whether the crime was reported in local media (0 = no, 1 = yes)
#       - age:  age of the victim
# ------------------------------------------------------------------------------
data("VictimsOfCrime", package = "gamlss.data")


str(VictimsOfCrime)

car::some(VictimsOfCrime)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

par(mfrow = c(1, 1))
plot(reported ~ age, data = VictimsOfCrime, pch = "|")




# Univariate logistic regression

popbio::logi.hist.plot(VictimsOfCrime$age, VictimsOfCrime$reported, col = "skyblue",
                       type =" hist", boxp = TRUE,  rug = TRUE, counts = FALSE)
