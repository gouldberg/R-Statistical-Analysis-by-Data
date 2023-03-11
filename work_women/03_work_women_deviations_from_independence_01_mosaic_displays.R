setwd("//media//kswada//MyFiles//R//work_women")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  work_women
# ------------------------------------------------------------------------------

work <- read.table("work_women.csv", header = TRUE, row.names = 1, sep = ";")

str(work)

dim(work)


colnames(work) <- c("stay.at.home", "part.time.work", "full.time.work", "totally.agree", "mostly.agree", "mostly.disagree", "totally.disagree")

names(work)

work



# ----------
# we divide into two tables
( work1 <- work[,1:3] )

( work2 <- work[,4:7] )



# ------------------------------------------------------------------------------
# mosaic display
#  - is similar to the sieve diagram. However, mosaic plots and related methods
#     - generalize more readily to n-way tables. One can usefully examine 3-way, 4-way, and even larger tables, subject to the limitations of resolution in any graph.
#     - are intimately connected to loglinear models, generalized linear models, and generalized non-linear models for frequency data
#     - provide a method for fitting a series of sequential loglinear models to the various marginal totals of an n-way table
#     - can be used to illustrate the relations among variables that are fitted by various loglinear models
# ------------------------------------------------------------------------------

vcd::mosaic(as.matrix(work1), shade=TRUE, suppress=0, labeling=labeling_residuals, gp_text=gpar(fontface=2))




