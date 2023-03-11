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
# linear regression for proportion data
# ------------------------------------------------------------------------------

psych::describe(mammalsleep)


# -->
# skewness of body, brain, lifespan, gestation > 1, so we apply log transformation



# ----------
linmod <- glm(pdr ~ log(body) + log(brain) + log(lifespan) + log(gestation) + predation + exposure + danger, family = gaussian, mammalsleep)


summary(linmod)



# -->
# only log(body) is significant ..