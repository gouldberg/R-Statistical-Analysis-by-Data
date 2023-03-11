setwd("//media//kswada//MyFiles//R//cigar")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cigar
#   - Number of cigaret boxes sold per person in each US state through 1970 - 1993.
#     Proposition99 is the no-smaking campaign, which started in 1988 in California,
#     imposing 25 cents tax for each cigaret box.
# ------------------------------------------------------------------------------

data("Cigar", package = "Ecdat")

dim(Cigar)

str(Cigar)


car::some(Cigar)


# ----------
# states To be excluded since tax for cigaret is increased by 50 cents or more from 1988.
# If those states are included, the assumption of Common Trend does not hold.
# Alaska, Hawaii, Maryland, Michigan, New Jersey, New York, Washington
skip_state <- c(3, 9, 10, 22, 21, 23, 31, 33, 48)


Cigar <- Cigar %>% filter(!state %in% skip_state, year >= 70) %>% mutate(area = if_else(state == 5, "CA", "Rest of US"))




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
