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
# data exploration:  Filtering by HoltWinters
# ------------------------------------------------------------------------------


HW_Nile <- HoltWinters(Nile, beta = FALSE, gamma = FALSE)


str(HW_Nile)



# ----------
HW_Nile$fitted



plot(HW_Nile, col = "darkgray", col.predicted = "black", lty.predicted = "dashed")
