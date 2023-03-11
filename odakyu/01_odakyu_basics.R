
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\odakyu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  odakyu
#   - housing (apartment, condominium) price along Odakyu-lines, listed on magazine "forent" (by Recruit) in Nov. and Dec. 1995
#   - Variables:
#       - price:   yen by monthly
#       - bus_min:  minutes by nearest bus station
#       - walk_min:  minutes by walk from nearest bus or train station
#       - shikirei:  num of months corresponding to shiki-rei kin
#       - area:  area m^2
#       - years:  years from construction completed
#       - floor:  0 for 1st floor and 1 for 2nd floor
#       - ac:  0 and 1 for without and with air-conditioning
#       - closet:  0 and 1 for without and with closets
#       - flooring:  0 and 1 for without and with flooring
#       - balcony:  0 and 1 for without and with balcony
#       - train_min:  minutes by train from Shinjuku
#       - num_exchange:  number of train exchanges at the station
#       - num_station:  number of stations from express-stopping station
#       - train_fare:  train fares from Shinjuku, one-way (yen)
#       - km:  distance from Shinjuku (km)
#       - station:  the name of nearest station
# ------------------------------------------------------------------------------

dat <- read.csv("odakyu_dat.txt", header = TRUE, sep = "\t")


str(dat)


car::some(dat)



# ----------
dat$floor <- as.factor(dat$floor)

dat$ac <- as.factor(dat$ac)

dat$closet <- as.factor(dat$closet)

dat$flooring <- as.factor(dat$flooring)

dat$balcony <- as.factor(dat$balcony)



# ----------
# total minutes
dat$min_tot <- dat$bus_min + dat$walk_min + dat$train_min


# price including train fares for 20 days per month
dat$price_tot <- dat$price + dat$train_fare * 20


# price per area
dat$price_ave <- dat$price / dat$area
dat$price_tot_ave <- dat$price_tot / dat$area





# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

