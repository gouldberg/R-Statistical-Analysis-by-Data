setwd("//media//kswada//MyFiles//R//ucb_admissions")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UCBAdmissions
#  - aggregate data on applicants to graduate school at berkeley for the six largest department in 1973 classified by admission and gender.
#  - The presence of an association might be considered as evidence of sex bias in admission practices.
# ------------------------------------------------------------------------------

data("UCBAdmissions", package = "datasets")


data <- UCBAdmissions


data


dimnames(data)


dim(data)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

