setwd("//media//kswada//MyFiles//R//ucb_admissions")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UCBAdmissions
# ------------------------------------------------------------------------------

data("UCBAdmissions", package = "datasets")


data <- UCBAdmissions


data


dimnames(data)


dim(data)




# ------------------------------------------------------------------------------
# Assess homogeneity of association (homofeneity of odds ratios)
# ------------------------------------------------------------------------------

woolf_test(data)



# -->
# The test is significant, indicating that the odds ratios cannot be considered equal across department

