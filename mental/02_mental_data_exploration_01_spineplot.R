setwd("//media//kswada//MyFiles//R//mental")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Mental
# ------------------------------------------------------------------------------
data("Mental", package = "vcdExtra")

data <- Mental

data



# ------------------------------------------------------------------------------
# spine plot
# ------------------------------------------------------------------------------
tab <- xtabs(Freq ~ ses + mental, data = Mental)

tab



# ----------
spineplot(tab)



# -->
# spine plot for conditional distribution of impairment, given the categories of SES
# stacked barchart of the row percentages of mental impairment for each SES category, 
# the  width of each bar being proportional to the overall SES percentages
# It is apparent that the "well" mental state decreases with social-economic status, while the "impaired" state increases.
