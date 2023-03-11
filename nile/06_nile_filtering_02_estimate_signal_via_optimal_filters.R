rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------

data(Nile)


dim(Nile)


str(Nile)



# ------------------------------------------------------------------------------
# Estimating Signal via Optimal Filters:  by astsa::sigExtract
# ------------------------------------------------------------------------------

# Performs signal extraction and optimal filtering
# L:  degree of smoothing
# M:  number of terms used in the lagged regression approximation
# max.freq:  truncation frequency, which must be larger than 1/M


astsa::SigExtract(Nile, L = 3, M = 10, max.freq = 0.1)

