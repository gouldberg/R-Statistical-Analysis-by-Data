setwd("//media//kswada//MyFiles//R//irrigation")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  irrigation
#   - In an agricultural field trial, the objective was to determine the effects of two crop varieties and four different irrigation methods.
#     Eight fields were available, but only one type of irrigation may be applied to each field. The fields may be divided into two parts with
#     a different variety planted in each half. The whole plot factor is the method of irrigation, which should be randomly assigned to the fields.
#     Within each field, the variety is randomly assigned.
#   - This is the design of "Split Plots". Note the distinction between split plots and blocks.
#        - Blocks are features of the experimental units whch we have the option to take advantage of in the experimental design.
#        - Split plots often arise in nonagricultural settings when one factor is easy to change while another factor takes much more time to change.
#          If the experimenter must do all runs for each level of the hard-to-change factor consecutively, a split-plot design results with
#          the hard-to-change factor representing the whole plot factor.
# ------------------------------------------------------------------------------

data("irrigation", package = "faraway")

str(irrigation)


car::some(irrigation)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
