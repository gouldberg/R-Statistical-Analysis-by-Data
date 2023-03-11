setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\film_process")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Film Extrusion Process
#   - The data arose from a planned experiment in which the Heater current and Valve position were adjusted
#     aproximately every 5 seconds, according to a table of random normal values.
#     In practice these input settings had to be occasionally modified to keep the process stable and the adjustments
#     were made slightly more slowly than the sampling interval of 5 seconds,
#     so there is some slight autocorrelation in their values.
#     The data were supplied by ICI Ltd. in 1968 for a PhD project and were scaled to give some commercial protection
#     as their analysis was publically available
#
#   - The data are from an open loop experiment
#   - Variables:
#        - Input:  heater current, valve setting
#        - Output:  extrusion pressure, width
# ------------------------------------------------------------------------------


film <- read.table("FilmProcess.txt", sep = "", header = F, colClasses = "numeric")


head(film)





# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

