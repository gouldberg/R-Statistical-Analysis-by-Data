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
# data exploration:  Time series decompose by HoltWinters
# ------------------------------------------------------------------------------


HW_Nile <- HoltWinters(Nile, beta = FALSE, gamma = FALSE)


str(HW_Nile)



# ----------
HW_out <- HW_Nile


# filtered and residuals only
HW_decomp <- ts.union(y = HW_out$x, Level = HW_out$fitted[, "level"], Residuals = residuals(HW_out))


plot(HW_decomp)
