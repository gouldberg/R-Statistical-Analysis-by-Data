# setwd("//media//kswada//MyFiles//R//coffee")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//coffee")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coffee
#   - A survey recorded the brand choice for a sample of buyers of instant coffee.
#     At a later coffee purchase by these subjects, the brand choice was again recorded.
#     This is the results for 5 brands of decaffeinated coffee.
# ------------------------------------------------------------------------------

Coffee <- read.table(file = "Coffee.dat", header = T, stringsAsFactors = FALSE)


str(Coffee)


car::some(Coffee)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
