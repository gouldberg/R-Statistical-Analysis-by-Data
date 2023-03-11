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



# ------------------------------------------------------------------------------
# Use quasi-binomial to model the proportion response diretcly
# ------------------------------------------------------------------------------

qlmod <- glm(pdr ~ log(body) + log(brain) + log(lifespan) + log(gestation) + predation + exposure + danger, family = quasibinomial, mammalsleep)


summary(qlmod)


faraway::sumary(qlmod)



# -->
# dispersion parameter = 0.4480

