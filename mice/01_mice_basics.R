setwd("//media//kswada//MyFiles//R//mice")


packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  mice
#   - In an experiment to assess the effects of selection for body weight on the shape of mouse vertevrae, three groups of mice were obtained:
#     Large, Small and Control.
#   - The Large group contains mice selected at each generation according to large body weight.
#     The Small group was selected for small body weight and the Control group contains unselected mice.
#   - The bones form part of a much larger study and these bones are from replicate E of the study
#
#   - We consider the second thoracic vertebra T2. There are 30 Control, 23 Lage and 23 Small bones.
#     Each vertebra was placed under a microscope and digitized using a video camera to give a grey level image.
#     The outline of the bone is then extracted using standard image processing techniques to give a stream of about 300 coordinates around the outline.
#     Six landmarks were taken from the outline using a semi-automatic procedure, where an approximate curvature function of the smoothed outline
#     is derived and the mathematical landmarks are placed at points of extreme curvature as measured by this function.
#
#   - The landmarks 1 and 2:  maximum points of approximate curvature function (usually at eh widest part of the vertebra rather than on the tips)
#     The landmarks 3 and 5:  at the extreme points of negative curvature at hte base of the spinous process
#     The landmarks 4:  at the tip of the spinous process
#     The landmarks 6:  at the maximal curvature point on the opposite side of the bone from 4
# ------------------------------------------------------------------------------

data(mice, package = "shapes")


dim(mice$x)



# ----------
# The coordinates in mice$x are also available individually by group
data(qcet2.dat, package = "shapes")
data(qlet2.dat, package = "shapes")
data(qset2.dat, package = "shapes")



# ------------------------------------------------------------------------------
# basic
# ------------------------------------------------------------------------------

