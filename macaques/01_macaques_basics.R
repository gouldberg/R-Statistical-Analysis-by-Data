setwd("//media//kswada//MyFiles//R//macaques")


packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  macaques
#   - In an investigation into sex differences in the crania of a species of macaque Macacafascicularis (a type of monkey), 
#     random samples of 9 male and 9 femanl skulls were obtained by Paul O'Higgins (Hull-Yourk Medical School).
#   - A total of 26 landmarks are displayed on the skull and a subset of 7 was taken for analysis.
#     A subset of seven anatomical landmarks was located on each cranium and the three-dimensional (3D) coordinates of each point were recorded.
#     The 7 chosen landmarks are: 1, prosthion; 7, opisthion; 10, bregma; 12, nasion; 15, asterion; 16, midpoint of xyg/temp suture; and 17, interfrontomalare
# ------------------------------------------------------------------------------

data(macaques, package = "shapes")
data(macf.dat, package = "shapes")
data(macm.dat, package = "shapes")

str(macaques)

dim(macaques$x)

macaques



# ----------
dim(macf.dat)

dim(macm.dat)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
