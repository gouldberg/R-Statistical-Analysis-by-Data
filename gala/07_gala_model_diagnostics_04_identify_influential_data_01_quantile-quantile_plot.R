setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
# mod_obj <- modp.step2
mod_obj <- modp2
# mod_obj <- mod.nbin



# ------------------------------------------------------------------------------
# Normal Quantile-quantile Plot:  Studentized residuals vs. Normal Quantiles
# ------------------------------------------------------------------------------

car::qqPlot(rstudent(mod_obj), xlab = "Normal Quantiles", ylab = "Studentized residuals")


# -->
# No outliers but Echidna is nearly outliers



# ----------
# by group
car::qqPlot(rstudent(mod_obj), group = gala$Species >= 20, xlab = "Normal Quantiles", ylab = "Studentized residuals")




# -->
# For GLMs with discrete responses, such plots are often disappointing, even with a reasonably good-fitting model, because
# (a) possible outlierrs can appear at both the lower and upper ends of the distribution of residuals
# (b) the theoretical normal distribution used to derive the envelope may not be well approximated in a given model


