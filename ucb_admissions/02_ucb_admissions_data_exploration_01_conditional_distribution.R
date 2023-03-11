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
# Conditional distribution
#  - When one variable (the column variable, B, for example) is a response variable, and the other (A) is an explanatory variable,
#    it is most often useful to examine the distribution of the reponse B for each level of A separately.
#    These define the conditional distributions of B, given the level of A.
# ------------------------------------------------------------------------------

dat_tab <- margin.table(data, 2:1)

dat_tab



# ----------
library(gmodels)


# 1st row within each cell:  the joint frequencies
# 3rd row: joint sample percentages 
# 2nd row: the conditional percentage of admission or rejection for males and females separately.

CrossTable(dat_tab, prop.chisq = FALSE, prop.c = FALSE, format = "SPSS")

