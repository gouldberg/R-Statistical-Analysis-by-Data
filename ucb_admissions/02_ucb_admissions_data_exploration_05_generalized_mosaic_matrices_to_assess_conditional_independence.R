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
# Generalized mosaic matrices
#  - type argument can be used to plot mosaics showing various kinds of independence relations
#      - "pairwise":  bivariate marginal relations, collapsed over all other variables
#      - "total":  mosaic plots for mututal independence
#      - "conditional":  mosaic plots for conditional independence given all other variables
#      - "joint":  mosaic plots for joint independence of all pairs of variable from the others
# ------------------------------------------------------------------------------

# The observed frequencies are the same in all these cells
# However, in the lower panels, the tiles are shaded according to models of joint independence, 
# while in the upper panels, they are shaded according to models of mutual independence.


pairs(data, space = 0.2, lower_panel = pairs_mosaic(type = "joint"), upper_panel = pairs_mosaic(type = "total"))




# ----------
# For this data, more useful to fit and display the models of conditional independence for each pair of row, column variables
# given the remaining one

pairs(data, space = 0.2, type = "conditional")




# -->
# The shading in the (1,2) and (2,1) panels shows the fit of the model [Admin, Dept] [Gender, Dept],
# which asserts that Admission and Gender are independent, given (controlling for) Department.

# Except for Department A, this model fits quite well, again indicating lack of gender bias

# The panel (1,3) and (3,1) shows the relation between admission and department controlling for gender,
# highlighting the differential admission rates across departments.


# The (2,3) and (3,2) panels show how men and women apply differentially to the various deparments.
# It can be seen that men apply in much greater numbers to deparments A and B, with higher admission rates,
# while women apply in greater numbers to the departments C-F, with the lowest overall rate of admission.



