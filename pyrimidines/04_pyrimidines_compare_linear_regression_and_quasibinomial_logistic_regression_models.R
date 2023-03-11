setwd("//media//kswada//MyFiles//R//pyrimidines")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pyrimidines
# ------------------------------------------------------------------------------

data("pyrimidines", package = "faraway")

str(pyrimidines)

car::some(pyrimidines)

# pyrimidines <- pyrimidines[-c(6,72),]



# ------------------------------------------------------------------------------
# Compare linear regression and quasi-binomial regression:  predicted values
# ------------------------------------------------------------------------------

tmp <- data.frame(pred_lin = predict(linmod), pred_ql = predict(qlmod, type = "response"))
tmp$dif <- tmp$pred_lin - tmp$pred_ql

round(head(tmp), digits = 4)


car::scatterplot(pred_lin ~ pred_ql, data = tmp)



# -->
# predicted values are not much different !

