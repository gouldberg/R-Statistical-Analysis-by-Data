setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)
str(Vietnam)


car::some(Vietnam)



# frequency form in dataframe to table
( vietnam.tab <- xtabs(Freq ~ sex + year + response, data = Vietnam) )



# ----------
# table form to case form
expand.dft(vietnam.tab)



# ------------------------------------------------------------------------------
# gpairs with mosaic matrices
# ------------------------------------------------------------------------------

# gpairs require case form

gpairs::gpairs(expand.dft(vietnam.tab), diag.pars = list(fontsize = 20), mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate = 1:4)))




# ------------------------------------------------------------------------------
# mosaic matrices by pairs
# ------------------------------------------------------------------------------

# pairs handle directly xtabs matrix

pairs(vietnam.tab, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))
