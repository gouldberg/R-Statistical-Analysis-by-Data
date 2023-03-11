setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


# frequency form in dataframe to table
( acci.tab <- xtabs(Freq ~ age + result + mode + gender, data = Accident) )



# ------------------------------------------------------------------------------
# gpairs with mosaic matrices
# ------------------------------------------------------------------------------

# gpairs require case form

gpairs::gpairs(expand.dft(acci.tab), diag.pars = list(fontsize = 20), mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate = 1:4)))




# ------------------------------------------------------------------------------
# mosaic matrices by pairs
# ------------------------------------------------------------------------------

# pairs handle directly xtabs matrix

pairs(acci.tab, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))
