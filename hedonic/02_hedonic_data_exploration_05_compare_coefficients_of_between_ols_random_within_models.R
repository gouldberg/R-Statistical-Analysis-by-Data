setwd("//media//kswada//MyFiles//R//hedonic")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hedonic
# ------------------------------------------------------------------------------

data("Hedonic", package = "plm")


str(Hedonic)


dim(Hedonic)


car::some(Hedonic)



# ----------
Hp <- pdata.frame(Hedonic, index = "townid")



# ------------------------------------------------------------------------------
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")

hfm <- mv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + blacks + lstat

sapply(models, function(x) coef(plm(hfm, data = Hp, model = x))["crim"])

sapply(models, function(x) coef(plm(hfm, data = Hp, model = x))["zn"])

sapply(models, function(x) coef(plm(hfm, data = Hp, model = x))["indus"])

sapply(models, function(x) coef(plm(hfm, data = Hp, model = x))["blacks"])


# -->
# Note that for "crim" and "indus", absolute value is maximum for between estimator,
# indicating, this is not individual-specific variable

# For "blacks", within estimator is large, indicating this variable is quite individual specific.

