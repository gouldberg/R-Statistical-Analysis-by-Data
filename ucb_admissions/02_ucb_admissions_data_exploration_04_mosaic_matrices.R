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
# Mosaic matrices
#   - The vcd packages extends pairs() generic function to mosaic matrices with methods for "table" and "structable" objects
# ------------------------------------------------------------------------------

# The mosaic matrix show all 2-way marginal relations
# Some additional arguments are used to control the details of labels for the diagonal and off-diagonal panels.

largs <- list(labeling = labeling_border(varnames = FALSE, labels = c(T, T, F, T), alternate_labels = FALSE))

dargs <- list(gp_varnames = gpar(fontsize = 20), offset_varnames = -1, labeling = labeling_border(alternate_labels = FALSE))

pairs(data, shade = TRUE, space = 0.25, diag_panel_args = dargs, upper_panel_args = largs, lower_panel_args = largs)



# --> 
# The panel (1,2) shows that Admission and Gender are strongly associated marginally.

# The panel (1,3) shows that Department A and B have the greatest overall admission rate, departments E and F the least.

# The panel (2,3) shows that men and women apply differentially to the various departments

# It can be seen that men apply in much greater numbers to departments A and B, with higher admission rates,
# while women apply in greater numbers to the departments C-F, with the lowest overall rate of admission


