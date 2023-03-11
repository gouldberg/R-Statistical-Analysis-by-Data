setwd("//media//kswada//MyFiles//R//criminal")

packages <- c("dplyr", "vcd", "vcdExtra", "logmult")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  criminal
#  - 4 * 5 table, number of men aged 15-19 charged with a criminal case for whom charges were dropped in Denmark from 1955 - 1958
# ------------------------------------------------------------------------------
data("criminal", package = "logmult")

data <- criminal

data

margin.table(data, c(2,1))



# ------------------------------------------------------------------------------
# Grouped barchart
# ------------------------------------------------------------------------------
barplot(margin.table(data, c(2,1)), legend = TRUE)




# ------------------------------------------------------------------------------
# spline plot:  a stacked barchart of the row percentages
# ------------------------------------------------------------------------------
spineplot(data)



# --> 
# It seems that dropping of charges in relation to age changed over the years recorded here.
