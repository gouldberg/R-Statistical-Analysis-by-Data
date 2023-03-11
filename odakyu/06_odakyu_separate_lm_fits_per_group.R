
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



# Tokyo and 23 wards
( station <- unique(dat$station) )

tokyo_flg <- c(rep(1,16), rep(0,7), rep(1,3))

ward23_flg <- c(rep(1,14), rep(0,12))


dat$tokyo_flg <- tokyo_flg[match(dat$station, station)]

dat$ward23_flg <- ward23_flg[match(dat$station, station)]

dat$tokyo_ward23_flg <- paste0(dat$tokyo_flg, dat$ward23_flg)

table(dat$tokyo_ward23_flg)




# ----------
mod0 <- lm(price ~ area + years + km + min_tot + shikirei + num_exchange + num_stations + train_fare +
             bus_flg + ac + closet + flooring + balcony + floor, data = dat)


mod1 <- lm(price ~ area + years + km + min_tot + shikirei + num_exchange + num_stations +
             bus_flg + ac + closet + balcony + floor + 
             closet : area + balcony : area, data = dat)

mod_step1

mod4 <- lm(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
             bus_flg + ac + closet + balcony + floor + 
             closet : area + balcony : area + ac : area + floor : area +
             num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
             num_exchange : min_tot + num_exchange + balcony : years + shikirei : area + shikirei : num_exchange, data = dat)



mod5 <- lm(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
             bus_flg + ac + closet + balcony + floor + 
             closet : area + ac : area + floor : area +
             num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
             num_exchange : min_tot + num_exchange + shikirei : area + shikirei : num_exchange, data = dat)




# ------------------------------------------------------------------------------
# Check the residual distribution by station for mod5
# ------------------------------------------------------------------------------

lattice::bwplot(dat$station ~ resid(mod5))


lattice::bwplot(dat$tokyo_flg ~ resid(mod5))


lattice::bwplot(dat$ward23_flg ~ resid(mod5))



# -->
# very different distribution and centered around zero




# ------------------------------------------------------------------------------
# basic analysis:  assess confidence intervals of terms by tokyo_flg and station
# ------------------------------------------------------------------------------

library(nlme)


# we apply lmList to estimate intercept and coefficient by each group

# by tokyo_flg
mod00 <- lmList(price ~ area + years + km + min_tot + shikirei + num_exchange + num_stations + train_fare +
             bus_flg + ac + closet + flooring + balcony + floor | tokyo_flg, data = dat)



print(mod00)


plot(intervals(mod00))



# -->
# the coefficient for are is very different between tokyo_flg = 0 and 1




# ----------
mod00_2 <- lmList(price ~ area + years + min_tot + shikirei + num_exchange + num_stations + train_fare +
                  bus_flg + ac + closet + flooring + balcony + floor | ward23_flg, data = dat)



print(mod00_2)


plot(intervals(mod00_2))

plot(intervals(mod00))




# -->
# more contrastingly differenent between ward23_flg = 0 and 1




# ----------
mod00_3 <- lmList(price ~ area + years + min_tot + shikirei + num_exchange + num_stations + train_fare +
                    bus_flg + ac + closet + flooring + balcony + floor | tokyo_ward23_flg, data = dat)



print(mod00_3)


plot(intervals(mod00_3))



# -->
# for ac, closet, flooring, balcony, tokyo_ward23_flg = '10' is different distribution from '11' and '00'




# ----------
# by station, alarm message is produced...
mod00_4 <- lmList(price ~ area + years + min_tot + shikirei +
                    bus_flg + ac + closet + flooring + balcony + floor | station, data = dat)



print(mod00_4)




# ------------------------------------------------------------------------------
# Separate lm fits by lmList for mod5 per station, per tokyo_ward23_flg
# ------------------------------------------------------------------------------

library(nlme)


mod5_lmlist <- lmList(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
                    bus_flg + ac + closet + balcony + floor + 
                    closet : area + ac : area + floor : area +
                    num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
                    num_exchange : min_tot + num_exchange + shikirei : area + shikirei : num_exchange | station, 
                    data = dat)


print(mod5_lmlist)



# -->
# many stations and many terms --> cannot estimate properly




# ----------
mod5_2_lmlist <- lmList(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
                        bus_flg + ac + closet + balcony + floor + 
                        closet : area + ac : area + floor : area +
                        num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
                        num_exchange : min_tot + num_exchange + shikirei : area + shikirei : num_exchange | tokyo_ward23_flg, 
                      data = dat)


print(mod5_2_lmlist)




# ------------------------------------------------------------------------------
# Separate lm fits per station by lmlist for mod5
# ------------------------------------------------------------------------------


# mod6, previously estimated
mod6 <- lm(price ~ area + years + min_tot + shikirei + num_stations +
             ac + closet + balcony + floor + 
             closet : area + ac : area + floor : area +
             num_exchange : area + num_exchange : num_stations + shikirei, data = dat)


summary(mod6)



# excluding num_stations and num_exchange since if limited to one station, those term has only one level.
mod6_ns <- lm(price ~ area + years + min_tot + shikirei + 
             ac + closet + balcony + floor + 
             closet : area + ac : area + floor : area, data = dat)


summary(mod6_ns)




# ----------
# separate lm fits per station
mod6_ns_lmlist <- lmList(price ~ area + years + min_tot + shikirei + 
                ac + closet + balcony + floor + 
                closet : area + ac : area + floor : area | station, data = dat)


print(mod6_ns_lmlist)




# ----------
# separate lm fits per tokyo_ward23_flg
mod6_ns_2_lmlist <- lmList(price ~ area + years + min_tot + shikirei + 
                           ac + closet + balcony + floor + 
                           closet : area + ac : area + floor : area | tokyo_ward23_flg, data = dat)


print(mod6_ns_2_lmlist)




# ------------------------------------------------------------------------------
# BY STATION MODEL:  Assess distribution of intercept
# ------------------------------------------------------------------------------


car::densityPlot(coef(mod6_ns_lmlist)[,"(Intercept)"])




# ---------
# sorting by descending order of Intercept
coef(mod6_ns_lmlist)


rownames(coef(mod6_ns_lmlist))

order((coef(mod6_ns_lmlist)[,"(Intercept)"]), decreasing = T)

rownames(coef(mod6_ns_lmlist))[order((coef(mod6_ns_lmlist)[,"(Intercept)"]), decreasing = T)]



# -->
# gotokuji, yoyogi_hachiman, umegaoka, kakiu, have large intercept
# yomiuri_land_mae, yoyogi_uehara, tamagawa_gakuen_mae, have small intercept

# not estimated for
# higashi_kitazawa, sangu_bashi



# ------------------------------------------------------------------------------
# BY STATION MODEL:  Assess distribution of coefficient for 'area'
# ------------------------------------------------------------------------------


car::densityPlot(coef(mod6_ns_lmlist)[,"area"])




# ------------------------------------------------------------------------------
# BY STATION MODEL:  Assess relationship between coefficients by station
# ------------------------------------------------------------------------------

# scatterplot of coefficient:  area  vs. Intercept

lattice::xyplot(coef(mod6_ns_lmlist)[,"area"] ~ coef(mod6_ns_lmlist)[,"(Intercept)"], cex = 1.5, pch = 20)



# -->
# suggesting that the larger the intercept, the smaller the coeff of area



# ----------
# scatterplot matrix of coefficient

graphics.off()

pairs(mod6_ns_lmlist, id = 0.1, adj = -0.5)


