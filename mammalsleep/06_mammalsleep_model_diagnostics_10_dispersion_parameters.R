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
qlmod <- glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, data = mammalsleep)


mod_obj <- qlmod



# ------------------------------------------------------------------------------
# Dispersion parameter
# ------------------------------------------------------------------------------

sum(residuals(mod_obj, type = "pearson") ^ 2/ mod_obj$df.residual)


sum(residuals(mod_obj2b, type = "pearson") ^ 2/ mod_obj2b$df.residual)



# -->
# quite under dispersion ..