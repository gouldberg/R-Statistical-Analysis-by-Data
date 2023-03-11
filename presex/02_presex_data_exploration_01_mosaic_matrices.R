setwd("//media//kswada//MyFiles//R//presex")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PreSex
# ------------------------------------------------------------------------------
data("PreSex", package = "vcd")

str(PreSex)

dim(PreSex)


PreSex



# ------------------------------------------------------------------------------
# structable:  flat representation of a high-dimensional contingency table constructed by recursive splits
# ------------------------------------------------------------------------------
structable(Gender + PremaritalSex + ExtramaritalSex ~ MaritalStatus, data = PreSex)




# ------------------------------------------------------------------------------
# Mosaic matrices
#   - The vcd packages extends pairs() generic function to mosaic matrices with methods for "table" and "structable" objects
# ------------------------------------------------------------------------------
# The mosaic matrix show all 2-way marginal relations
# The diagonal panels show the labels for the category levels as well as the one-way marginal totals.
pairs(PreSex, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))



# -->
# If we view gender, premarital sex, and extramarital sex as explanatory, and marginal status as the response,
# then the mosaics in row 1 (and in column 1) show how marital status depends on each predictor marginally.



# -->
# Marital status is independent of gender (all residuals equal zero), by design of the data collection

# In the (1,3) panel, we see that reported premarital sex is more often followed by divorce,
# while non-report is more prevalent among those still married.

# The (1,2) panel shows a similar, but stronger relation between extramarital sex and marriage stability.

# --> 
# These effects pertain to the associations of P and E with marital status (M) -- the terms [PM] and [EM] in the loglinear model.


# -->
# The total amount of the shading in all the individual mosaics portrays the total associations decomposed by multiple correspondence analysis


