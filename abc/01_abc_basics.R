setwd("//media//kswada//MyFiles//R//abc")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ABC
#   - Customer satisfaction data using six items (Kenett and Salini, 2012):
#     equipment, sales, technical suppert, training, purchasing support, and pricing.
#   - All these items are scored on a 5-point rating scale indicating the customer satisfaction level (from 1 ... "very low" to 5 ... "very high").
# ------------------------------------------------------------------------------

data("ABC", package = "MPsychoR")

str(ABC)

dim(ABC)

car::some(ABC)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------



