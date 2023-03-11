setwd("//media//kswada//MyFiles//R//hsb")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hsb
# ------------------------------------------------------------------------------

data("hsb", package = "faraway")


str(hsb)


head(hsb)



# ------------------------------------------------------------------------------
# Model performance:  Accuracy
# ------------------------------------------------------------------------------

xtabs(~ predict(mmodi) + hsb$prog)


xtabs(~ predict(phmodi) + hsb$prog)


xtabs(~ predict(pomodi) + hsb$prog2)


# -->
# not good at for "general"


