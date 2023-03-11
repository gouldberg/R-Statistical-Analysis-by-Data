setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ------------------------------------------------------------------------------
# data exploration:  bivariate variable relation
# ------------------------------------------------------------------------------

var <- c("intensity", "prevalence", "area", "year", "length", "weight")


library(gpairs)


# marginal and conditional plots
gpairs(CodParasites[,var],
       diag.pars = list(fontsize = 16),
       mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate=1:4)),
       outer.rot = c(45,45))



