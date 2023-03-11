setwd("//media//kswada//MyFiles//R//lahman")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS", "Lahman")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Master
#  - The Lahman package contains comprehensive data on baseball statistics for Major League Baseball from 1871 through 2012.
#    For all players, the Master table records the handedness of players, in terms of throwing (L, R) and batting (B, L, R), where B indicates "both".
#
# data: Fielding
#
# ------------------------------------------------------------------------------
data("Master", package = "Lahman")
data("Fielding", package = "Lahman")


basehands <- with(Master, table(throws, bats))
basehands


MasterFielding <- data.frame(merge(Master, Fielding, by = "playerID"))
car::some(MasterFielding)


throwPOS <- with(MasterFielding, table(POS, throws))
throwPOS


