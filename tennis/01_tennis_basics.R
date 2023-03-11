# setwd("//media//kswada//MyFiles//R//tennis")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//tennis")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tennis
#   - The results of matches among five professional tennis players between January 2014 and January 2018.
# ------------------------------------------------------------------------------

Tennis <- read.table(file = "Tennis.dat", header = T, stringsAsFactors = FALSE)


str(Tennis)


Tennis


# -->
# Roger Federer won 6 of the 15 matches that he and Novak Djokovic played.



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
