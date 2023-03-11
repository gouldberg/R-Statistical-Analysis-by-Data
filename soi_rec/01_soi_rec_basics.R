# setwd("//media//kswada//MyFiles//R//soi_rec")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
#   - Southern Oscillation Index (SOI), which measures changes in air pressure, related to sea surface temperatures in the central Pacific Ocean.
#     The central pacific warms every 3 to 7 years due to the El Nino effect, which has bee blamed for various global extreme weather events.
#
# data:  rec
#   - associated Recruitment (number of new fish)
# Both data are furnished by Dr. Roy Mendelssohn of the Pacific Environmental Fisheries Group (personal communication).
# Both series are for a period of 453 months ranging over the years 1950 - 1987.
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")




# ----------
is.ts(soi)

is.ts(rec)


# start, end, period
tsp(soi)

tsp(rec)



# ----------
head(soi, 120)

head(rec, 120)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

