setwd("//media//kswada//MyFiles//R//eustockmarkets")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EuStockMarkets
# ------------------------------------------------------------------------------
data("EuStockMarkets", package = "datasets")


str(EuStockMarkets)

head(EuStockMarkets)



# ----------
dax <- EuStockMarkets[, "DAX"]

Rdax <- diff(log(dax))



# ------------------------------------------------------------------------------
# Residual diagnostics
# ------------------------------------------------------------------------------

mf


plot(mf, ts = TRUE)



# -->
# Note that kurtosis is almost 3.0  (close to normal distribution)



# ----------
wp(mf)

wp(mf, ylim.all = 1.0)

