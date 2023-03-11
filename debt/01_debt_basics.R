setwd("//media//kswada//MyFiles//R//debt")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  debt
#   - data arise from a large postal survey on the psychology of debt.
#     The frequency of creidt card use is a three-level factor ranging from never, through occasionally to regularly
#   - "ccarduse":  how ofen did s/he use credit cards (1: never, 2: occasionally, 3: regularly)
#   - "prodebt":  score on a scale of attidudes to debt (high values = favourable to debt)
#   - "incomegp":  income group (1: lowest, 5: highest)
# ------------------------------------------------------------------------------

data("debt", package = "faraway")


str(debt)


head(debt)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
