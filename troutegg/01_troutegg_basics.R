setwd("//media//kswada//MyFiles//R//troutegg")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  troutegg
#   - In Manly (1978), an experiment is reported where boxes of trout eggs were buried at five different stream locations and
#     retrieved at four different times, specified by the number of weeks after the original placement.
#     The number of surviving effs was recorded. The box was not returned to the stream.
# ------------------------------------------------------------------------------

data("troutegg", package = "faraway")

str(troutegg)

head(troutegg)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

xtabs(cbind(survive, total) ~ location + period, troutegg)

ftable(xtabs(cbind(survive, total) ~ location + period, troutegg))



# -->
# Notice that in one case, all the eggs survive, while in another, none of the eggs survive.



