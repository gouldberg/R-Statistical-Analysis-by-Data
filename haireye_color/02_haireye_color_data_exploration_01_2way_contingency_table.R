setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data



# ------------------------------------------------------------------------------
# Grouped barchart
# ------------------------------------------------------------------------------
# collapse table to 2-way table (Eye(2) and Hair(1))
( hec <- margin.table(data, 2:1) )


barplot(hec, beside = TRUE, legend = TRUE)



# --> Bar graphs do not extend well to more than one dimension



# ------------------------------------------------------------------------------
# tile plot
# ------------------------------------------------------------------------------
# somewhat better approach is tile plot
vcd::tile(hec, shade=TRUE)


# --> The table frequencies are represented by the area of rectangles arranged in the same tabular from as the raw data,
# facilitating comparisons between tiles across both variables (by rows and by columns)



# ------------------------------------------------------------------------------
# spline plot:  a stacked barchart of the row percentages
# ------------------------------------------------------------------------------
spineplot(hec)


