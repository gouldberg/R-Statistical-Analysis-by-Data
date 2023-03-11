setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# Mosaic matrices
#   - The vcd packages extends pairs() generic function to mosaic matrices with methods for "table" and "structable" objects
# ------------------------------------------------------------------------------

# The mosaic matrix show all 2-way marginal relations
# Some additional arguments are used to control the details of labels for the diagonal and off-diagonal panels.

largs <- list(labeling = labeling_border(varnames = FALSE, labels = c(T, T, F, T), alternate_labels = FALSE))

dargs <- list(gp_varnames = gpar(fontsize = 20), offset_varnames = -1, labeling = labeling_border(alternate_labels = FALSE))

pairs(dat2, shade = TRUE, space = 0.25, diag_panel_args = dargs, upper_panel_args = largs, lower_panel_args = largs)



