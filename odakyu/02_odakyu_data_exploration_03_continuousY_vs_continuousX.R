
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
# data exploration:  continuousY vs. continousX
# price ~ tot_min
# ------------------------------------------------------------------------------


library(ggplot2)

gg <- ggplot(dat, aes(min_tot, price)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg



# ----------
gg <- ggplot(dat, aes(min_tot, price_tot_ave)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg




# ----------

gg + facet_grid(~ ac)

gg + facet_grid(~ balcony)

gg + facet_grid(~ flooring)

gg + facet_grid(~ floor)

gg + facet_grid(~ closet)

gg + facet_grid(~ bus_flg)




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# price ~ km
# ------------------------------------------------------------------------------


gg <- ggplot(dat, aes(km, price)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg




# ------------------------------------------------------------------------------
# data exploration:  continuousY vs. continousX
# price ~ years
# ------------------------------------------------------------------------------


library(ggplot2)

gg <- ggplot(dat, aes(years, price)) + geom_point(size = 1, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) + 
  stat_smooth(method = "loess", col = "red") + stat_smooth(method = "lm")


gg


