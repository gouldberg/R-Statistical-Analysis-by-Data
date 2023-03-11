
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
# model diagnostics:  residuals
# ------------------------------------------------------------------------------

car::residualPlots(mod0)

car::residualPlots(mod1)

car::residualPlots(mod_step1)

car::residualPlots(mod4)

car::residualPlots(mod5)



# -->
# mod_step1 and mod4 is better than mod0 and mod1



# ----------
par(mfrow = c(2,2))

plot(mod5)





# ------------------------------------------------------------------------------
# identify influential observations
# ------------------------------------------------------------------------------

infl <- influence.measures(mod5)

infl




# ----------
library(car)

par(mfrow = c(1,1))

car::influencePlot(mod4, 
                   id=list(method="noteworthy", n=10, cex=1, col=carPalette()[1], location="lr"))


idx <- c(54, 166, 180, 253, 378, 405, 406, 640, 744, 761)

dat[idx,]


# compare to predicted price
predict(mod5)[idx]



# ----------
# 406:  seijo_gakuen_mae:  area is large, ac = 1, shikirei = 6, 
# min_tot is large (walk_min = 18 !!) --> price is relatively cheap

dat[c(406),]

summary(mod100 <- update(mod5, . ~ . + walk_min))



# ----------
# 405:  seijo_gakuen_mae:  no air-conditioning + large area, but price is high
# area coeff is negative and ac:area coeff is positive

dat[c(405),]


xtabs(~ station + ac, data = dat)

xtabs(~ station + shikirei, data = dat)

with(dat, by(area, station, mean))



# ----------
# 744:  seijo_gakuen_mae:  no air-conditioning + large area, but price is high
# area coeff is negative and ac:area coeff is positive

dat[c(744),]

predict(mod5)[idx]



# ----------
# 54:  yoyogi_uehara:  shikirei is 6 month !!  area is large 

dat[c(54),]

with(dat, by(area, station, mean))



# ----------
# 180:  shimo_kitazawa:  area is only 14

dat[c(180),]

with(dat, by(area, station, mean))



# ----------
# 378:  soshigaya_okura:  floor = 1 

dat[c(378),]

predict(mod5)[idx]




# ----------
# additional check

plot(dat$price ~ dat$shikirei)




# ------------------------------------------------------------------------------
# model diagnostics:  added-variable plots
# ------------------------------------------------------------------------------


par(mfrow=c(1,1))


car::avPlots(mod5, id = TRUE)




# ------------------------------------------------------------------------------
# Individual index plot:  scatterplot matrix of DFBETAs
#   - show the pairwise changes in the regression coefficients for the various predictors
# ------------------------------------------------------------------------------


head(infl$infmat)


dfbetas <- data.frame(infl$infmat[,2:22])


names(dfbetas)



# method = "mahl" to label the most extreme observations according to the Mahalanobis distance of each point from the centroid in the plot.

dfbetas_obj <- c("dfb.area", "dfb.yers", "dfb.mn_t", "dfb.shkr", "dfb.nm_x", "dfb.nm_s")


car::scatterplotMatrix(dfbetas[dfbetas_obj], smooth = FALSE, id = TRUE, 
                       showLabels=list(method = "mahal", n = 2, cex = 1.2, location = "lr"), 
                       ellipse = TRUE, levels = 0.95, robust = FALSE, 
                       diagonal = "histogram", col = gray(0.6))

# -->
# The joint effect of observations on pairs of coefficients is more complex than is apparent from the univariate views that appear in the plots along the diagonal.







