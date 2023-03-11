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



# ------------------------------------------------------------------------------
# mosaic display
# showing the relation of batting and throwing handedness, split first by batting and then by throwing
# ------------------------------------------------------------------------------
mosaic(margin.table(basehands, 2:1), gp = shading_Friendly2)


# --> 
# Most of the players who throw with their left hands bat with their left hands
# Most of the players who throw with their right hands bat with their right hands



# ------------------------------------------------------------------------------
# doubledecker()
# mosaic display of throwing hand vs fiedling position
# ------------------------------------------------------------------------------
round(prop.table(throwPOS), digits = 3)


# ----------
# decreasing order by left-handers
POS_order <- c("P","1B","OF","2B","3B","C","SS")
throws_order <- c("S","R", "L")



# ---------
# doubledecker()
doubledecker(throwPOS[POS_order, throws_order])



# ----------
# mosaic plot
mosaic(throwPOS[POS_order, throws_order], gp = shading_Friendly2)


