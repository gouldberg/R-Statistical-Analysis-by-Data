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
# Standardized Pearon residual by group
# ------------------------------------------------------------------------------

# group by against linear predictor

residualPlot(mod_obj, type = "rstandard", groups = mammalsleep$danger, linear = FALSE)
