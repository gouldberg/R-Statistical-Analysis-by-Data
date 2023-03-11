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
# spine plot for conditional distribution of impairment, given the categories of SES
# ------------------------------------------------------------------------------
tab <- xtabs(Freq ~ ses + mental, data = Mental)

tab


# spine plot for conditional distribution of impairment, given the categories of SES
# stacked barchart of the row percentages of mental impairment for each SES category, 
# the  width of each bar being proportional to the overall SES percentages
spineplot(tab)


# --> It is apparent that the "well" mental state decreases with social-economic status, while the "impaired" state increases.




# ------------------------------------------------------------------------------
# Cross table:  conditional and joint distribution
# ------------------------------------------------------------------------------
# 1st row within each cell:  the joint frequencies
# 3rd row: joint sample percentages 
# 2nd row: the conditional percentage of admission or rejection for males and females separately.
gmodels::CrossTable(tab, prop.chisq = FALSE, prop.c = FALSE, format = "SPSS")