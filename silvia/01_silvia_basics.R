# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\silvia")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  silvia
#   - second car price data of 244 Nissan Silvias, listed on magazine "Car Sensor" (by Recruit) in Nov. 1993
#   - Variables:
#       - P:  second car price (10 thousand yen)
#       - TM:  Transmission Type (0: Automatic,  1: Manual)
#       - Yr:  Years since registered as new car
#       - CT:  remained months to next car test
#       - NP:  car price at brand-new car  (10 thousand yen)
#       - DEAL:  via dealers (0: non-dealers,  1: dealers)
#       - KM:  running miles (10 thousand KM),  zero means that data is not given
# ------------------------------------------------------------------------------

dat <- read.csv("silvia_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)



# ----------
dat$TM <- as.factor(dat$TM)


dat$DEAL <- as.factor(dat$DEAL)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

