setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ----------
summary(qlmod)
summary(qlmod2b)



# ----------
qlmod <- glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, data = mammalsleep)


mod_obj <- qlmod
# mod_obj <- qlmod2b



# ------------------------------------------------------------------------------
# Pearon residual, Standardized Pearson residuals with curvature test
#   - Plots the residuals versus each term in a mean function and versus fitted values
#   - Also computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero
#   - For linear models, this is Tukey's test for nonadditivity when plotting against fitted values
# ------------------------------------------------------------------------------

residualPlots(mod_obj)


residualPlots(mod_obj, type = "rstandard")




# ------------------------------------------------------------------------------
# Standardized Pearson Residual with curvature test
# ------------------------------------------------------------------------------

# residuals against each predictor to check systematic variation
# quadratic = TRUE:  computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero

car::residualPlot(mod_obj, "log(body)", type = "rstandard", 
                  quadratic = TRUE, col.quad = gray(0.7), ylim = c(-5, 5))


car::residualPlot(mod_obj, "log(lifespan)", type = "rstandard", 
                  quadratic = TRUE, col.quad = gray(0.7), ylim = c(-5, 5))



