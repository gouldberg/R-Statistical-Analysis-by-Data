
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\odakyu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  odakyu
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


# bus
dat <- dat %>% mutate(bus_flg = ifelse(bus_min > 0, 1, 0))
dat$bus_flg <- as.factor(dat$bus_flg)



# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. categoryX
# price_ave ~ floor, balcony, ac, closet, flooring
# ------------------------------------------------------------------------------


boxplot(price_ave ~ floor, data = dat, varwidth = TRUE)

t.test(price_ave ~ floor, data = dat)



# ----------
boxplot(price_ave ~ balcony, data = dat, varwidth = TRUE)

t.test(price_ave ~ balcony, data = dat)



# ----------
boxplot(price_ave ~ ac, data = dat, varwidth = TRUE)

t.test(price_ave ~ ac, data = dat)



# ----------
boxplot(price_ave ~ flooring, data = dat, varwidth = TRUE)

t.test(price_ave ~ flooring, data = dat)



# ----------
# this is NOT SIGNIFICANT (price_ave)

boxplot(price_ave ~ closet, data = dat, varwidth = TRUE)

t.test(price_ave ~ closet, data = dat)


# this IS IGNIFICANT (price)
t.test(price ~ closet, data = dat)




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. categoryX
# price_ave ~ num_exchange
# ------------------------------------------------------------------------------


# lower price for 3 exchanges
boxplot(price_ave ~ num_exchange, data = dat, varwidth = TRUE)


# No significant trend
boxplot(price_ave ~ num_stations, data = dat, varwidth = TRUE)




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. categoryX
# price_ave ~ station
# ------------------------------------------------------------------------------


boxplot(price_ave ~ station, data = dat, varwidth = TRUE)




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. categoryX
# price_ave ~ shikirei
# ------------------------------------------------------------------------------


boxplot(price_ave ~ shikirei, data = dat, varwidth = TRUE)


