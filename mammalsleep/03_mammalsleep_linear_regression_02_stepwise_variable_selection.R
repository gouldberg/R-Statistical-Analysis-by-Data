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
# Stepwise variable seleciton
# ------------------------------------------------------------------------------

# step() select model by AIC
linmod2 <- step(linmod, direction = "both")


summary(linmod2)



# -->
# only body, lifespan, gestation, and danger are remained,
# which variables are selected from segment of variable clustering Hoeffding D's tree

plot(vc)



# ----------
summary(linmod2)



# ---------
anova(linmod, linmod2, test = "Chisq")



# -->
# not much different from full model.

