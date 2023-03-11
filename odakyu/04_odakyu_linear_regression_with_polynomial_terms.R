
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
# including poly(,2) term
# ------------------------------------------------------------------------------


mod0 <- lm(price ~ area + years + km + min_tot + shikirei + num_exchange + num_stations + train_fare +
             bus_flg + ac + closet + flooring + balcony + floor, data = dat)


summary(mod0)



# ----------
# no interaction, only significant term model

mod0_1 <- lm(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
               bus_flg + ac + closet + floor, data = dat)

summary(mod0_1)



# exluding bus_flg
mod0_1 <- lm(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
               ac + closet + floor, data = dat)

summary(mod0_1)


car::residualPlots(mod0_1)




# ----------
# including 2nd order polynomial term to area, min_tot, shikirei, and num_stations

mod0_2 <- lm(price ~ poly(area, 2) + years + poly(min_tot, 2) + poly(shikirei, 2) + num_exchange + poly(num_stations,2) +
               ac + closet + floor, data = dat)


# excluding closet, and include bus_flg, and balcony
mod0_2 <- lm(price ~ poly(area, 2) + years + poly(min_tot, 2) + poly(shikirei, 2) + num_exchange + poly(num_stations,2) +
               ac + floor + balcony + bus_flg, data = dat)


summary(mod0_2)


car::residualPlots(mod0_2)



# ----------
# compare with mod5
car::residualPlots(mod5)




# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------

AIC(mod0_1, mod0_2)

AIC(mod5, mod0_2)

summary(mod5)

summary(mod0_2)



# -->
# AIC:  mod5 is better
# multiple R-squared:  mod5 is better



# ----------
stargazer::stargazer(mod5, mod0_2, type = "text")



# -->
# coefficient for shikirei is very different




# ------------------------------------------------------------------------------
# excluding shikirei's interaction term
# ------------------------------------------------------------------------------

# excluding shikirei's interaction term

mod6 <- lm(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
             bus_flg + ac + closet + balcony + floor + 
             closet : area + ac : area + floor : area +
             num_exchange : area + num_exchange : num_stations + 
             num_exchange : min_tot + num_exchange, data = dat)


# excluding other non-significant term
mod6 <- lm(price ~ area + years + min_tot + shikirei + num_stations +
             ac + closet + balcony + floor + 
             closet : area + ac : area + floor : area +
             num_exchange : area + num_exchange : num_stations, data = dat)


summary(mod6)


car::Anova(mod6)


# -->
# area is non significant but interaction terms are significant




# ----------
AIC(mod5, mod6, mod0_2)



# -->
# still mod5 is best


car::residualPlots(mod6)





