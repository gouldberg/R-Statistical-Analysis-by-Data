setwd("//media//kswada//MyFiles//R//arthritis")

packages <- c("dplyr", "vcd", "MASS", "datasets", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

data <- Arthritis

data



# ------------------------------------------------------------------------------
# Generalized pairs plot
#
#  - gpairs() provides a variety of options for the CQ and QQ combinations, as well as the diagonal cells.
#  - The cobbination of 2 variables, each of which can be either of categorical (C) or quantitative (Q)
#      - CC:  mosaic display, sieve diagram, doubledecker plot, faceted or divided bar chart
#      - CQ:  side-by-side boxplots, stripplots, faceted histograms, aligned density plots
#      - QQ:  scatterplot, corrgram, data ellipses, etc.
# ------------------------------------------------------------------------------#
# only the defaults are used here
#  - The bottom row, corresponding to Age, uses boxplots to show the distributions of age for each of the categorical variables
#  - The last column shows these same variables as stripplots (or "barcodes"), which show all the individual observations.

gpairs(data[,c(5, 2, 3, 4)], diag.pars = list(fontsize = 20), mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate = 1:4)))



