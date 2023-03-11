setwd("//media//kswada//MyFiles//R//arthritis")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
#  - Data from Koch & Edwards (1988) from a double-blind clinical trial investigating a new treatment for rheumatoid arthritis.
#  - There are two explanatory factors: Treatment and Sex. Age is a numeric covariate, and Improved is the response
#    -- an ordered factor, with levels "None" < "Some" < "Marked"
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

data <- Arthritis

data



# ----------
# case form to table form
tab <- table(data[,c("Treatment", "Sex", "Improved")])
tab


ftable(tab)



# ----------
# table from back to data.frame in case form
data_df <- expand.dft(tab)
data_df




