
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
# M-estimation by Huber method
# ------------------------------------------------------------------------------

library(MASS)


# all 2-way interactions are accounted
rlmod <- rlm(price ~ (area + min_tot + km + years + shikirei + train_fare + num_exchange + num_stations +
                        bus_flg + floor + ac + closet + flooring + balcony)^2, data = dat)


summary(rlmod)



# -->
# R2 statistics is not given because it does not make sense in the context of a robust regression
# p-values are not given, too.
# but t-value for income, type : income is large

# The numerical values of the coefficients have changed somewhat and the standard errors are generally smaller.

# Although the robust fit gives numerically different output,
# the overall impression of what predictors are significant in explaining the response is unchanged.
# Thus the robust regression has provided some measure of confirmation.




# ------------------------------------------------------------------------------
# weights assigned by the model 
# ------------------------------------------------------------------------------

wts <- rlmod$w


dat[which(wts < 0.3),]





# ------------------------------------------------------------------------------
# compare:  original price,  predicted price by mod5 and rlmod
# ------------------------------------------------------------------------------

idx <- c(54, 166, 180, 253, 378, 405, 406, 640, 744, 761)


wts[idx]


# --> surprisingly 405, 406, 640 are weighted by 1  (405, 406, 640 have large hat values)



tmp <- data.frame(id = idx, price = dat$price[idx], mod5_price = predict(mod5)[idx], rlmod_price = predict(rlmod)[idx])


tmp <- tmp %>% mutate(dif1 = mod5_price - price, dif2 = rlmod_price - price)


tmp


# -->
# some values gets close to original price
# especially, 405, 406, 761

