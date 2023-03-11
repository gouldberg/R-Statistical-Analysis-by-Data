
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
# mod5 by gls
# ------------------------------------------------------------------------------


library(nlme)


mod5.form <- formula(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
                      bus_flg + ac + closet + balcony + floor + 
                      closet : area + ac : area + floor : area +
                      num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
                      num_exchange : min_tot + num_exchange + shikirei : area + shikirei : num_exchange)



gls5 <- gls(mod5.form, data = dat)



# ----------
summary(gls5)


# -->
# residual standard error = 7491.191


printCoefmat(coef(summary(gls5)))


# p-value are only reference information




# ------------------------------------------------------------------------------
# model fixed variance structure but increased by area
# var(e) = sigma^2 * area
# ------------------------------------------------------------------------------


# varFixed(~ area) allows for larger residual spread if area increases
# no extra parameters involved !!

gls6 <- gls(mod5.form, weights = varFixed(~ area), data = dat)


summary(gls6)


# -->
# residual standard error = 1615.967



printCoefmat(coef(summary(gls6)))



# ----------
# REML-based LR test of homoscedasticity
anova(gls5, gls6)




# ------------------------------------------------------------------------------
# model variance by station
# var(e(station)) = sigma^2(station))
# ------------------------------------------------------------------------------


# residual spread differs per station
# varIdent(form = ~1 | station)

gls7 <- gls(mod5.form, weights = varIdent(form = ~ 1 | station), data = dat)

printCoefmat(coef(summary(gls7)))



gls7_2 <- gls(mod5.form, weights = varIdent(form = ~ 1 | tokyo_ward23_flg), data = dat)

printCoefmat(coef(summary(gls7_2)))




# ----------
# REML-based LR test of homoscedasticity
anova(gls5, gls7)

anova(gls5, gls7_2)



# -->
# gls7: 25 more parameters, but better



# ----------
# variance function structure
gls7$modelStruct$varStruct



# -->
# residual standard error = 2852.003
# and those are multiplication factors
# note that minami_shinjuku is smallest 1.0 ... interesting






# ----------
plot(gls5, which = c(1), col = dat$station, ylim = c(-5, 7))

plot(gls7, which = c(1), col = dat$station, ylim = c(-5, 7))

plot(gls7_2, which = c(1), col = dat$station, ylim = c(-5, 7))

coplot(resid(gls7) ~ area | station, data = dat)



# -->
# not much different ...




# ----------
# 95% CI for variance function
intervals(gls7, which = "var-cov")




# ----------
# compare coefficient
tmp <- data.frame(gls5 = coef(gls5), gls7 = coef(gls7))

tmp <- tmp %>% mutate(diff = round((gls7 - gls5) / gls5, 3))

rownames(tmp) <- names(coef(gls5))

tmp



# -->
# num_exchange is uncertain ..




# ------------------------------------------------------------------------------
# model varPower variance structure by station, increasing by area
# var(e) = sigma^2 * abs(area(station)) ^ (2 * delta)
# ------------------------------------------------------------------------------


# varPower(form = ~ area | station)

gls8 <- gls(mod5.form, weights = varPower(form = ~ area | station), data = dat)

printCoefmat(coef(summary(gls8)))

# only significant terms
formula <- price ~ area + years + min_tot + shikirei + num_stations + bus_flg + ac + balcony +
  ac : area + ac : floor + num_exchange : num_stations + shikirei : train_fare + 
  min_tot : num_exchange + shikirei : area + shikirei : num_exchange

gls8 <- gls(formula, weights = varPower(form = ~ area | station), data = dat)

printCoefmat(coef(summary(gls8)))




# ----------
gls8_2 <- gls(formula, weights = varPower(form = ~ area | tokyo_ward23_flg), data = dat)

printCoefmat(coef(summary(gls8_2)))




# ----------
# REML-based LR test of homoscedasticity
anova(gls5, gls8)



# -->
# gls8: 25 + 1 more parameters



# ----------
# variance function structure
gls8$modelStruct$varStruct




# ----------
plot(gls7, which = c(1), col = dat$station, ylim = c(-5, 7))

plot(gls8, which = c(1), col = dat$station, ylim = c(-5, 7))

plot(gls8_2, which = c(1), col = dat$station, ylim = c(-5, 7))

coplot(resid(gls8) ~ area | station, data = dat)





# ----------
# 95% CI for variance function
intervals(gls8, which = "var-cov")




# ------------------------------------------------------------------------------
# model varExp variance structure by station, increasing by area
# var(e) = sigma^2 * exp(2 * delta * area(station))
# ------------------------------------------------------------------------------

gls9 <- gls(mod5.form, weights = varExp(form = ~ area | station), data = dat)

printCoefmat(coef(summary(gls9)))

# only significant terms
formula <- price ~ area + years + min_tot + shikirei + num_stations + bus_flg + ac + balcony +
  ac : area + ac : floor + num_exchange : num_stations + shikirei : train_fare + 
  min_tot : num_exchange + shikirei : area + shikirei : num_exchange

gls9 <- gls(formula, weights = varPower(form = ~ area | station), data = dat)

printCoefmat(coef(summary(gls9)))




# ----------
# variance function structure
gls9$modelStruct$varStruct




# ----------
plot(gls9, which = c(1), col = dat$station, ylim = c(-5, 7))

coplot(resid(gls9) ~ area | station, data = dat)




# ------------------------------------------------------------------------------
# model varComb variance structure by station, increasing by area
# var(e) = sigma^2(station) * exp(2 * delta * area)
# ------------------------------------------------------------------------------

vf10 <- varComb(varIdent(form = ~1 | station), varExp(form = ~ area))

gls10 <- gls(mod5.form, weights = vf10, data = dat)

printCoefmat(coef(summary(gls10)))


# only significant terms
formula <- price ~ area + years + min_tot + shikirei + num_stations + bus_flg + ac + balcony +
  ac : area + ac : floor + num_exchange : num_stations + shikirei : train_fare + 
  min_tot : num_exchange + shikirei : area + shikirei : num_exchange

gls10 <- gls(formula, weights = vf10, data = dat)

printCoefmat(coef(summary(gls10)))




# ----------
vf10_2 <- varComb(varIdent(form = ~1 | tokyo_ward23_flg), varExp(form = ~ area))

gls10_2 <- gls(mod5.form, weights = vf10_2, data = dat)

printCoefmat(coef(summary(gls10_2)))

# only significant terms
formula <- price ~ area + years + min_tot + shikirei + num_stations + bus_flg + ac + balcony +
  ac : area + ac : floor + num_exchange : num_stations + shikirei : train_fare + 
  min_tot : num_exchange + shikirei : area + shikirei : num_exchange

gls10_2 <- gls(formula, weights = vf10_2, data = dat)

printCoefmat(coef(summary(gls10_2)))



# ----------
# variance function structure
gls10$modelStruct$varStruct




# ----------
graphics.off()

plot(gls5, which = c(1), col = dat$station, ylim = c(-5, 7))

plot(gls10, which = c(1), col = dat$station, ylim = c(-5, 7))

coplot(resid(gls10) ~ area | station, data = dat)




# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------


AIC(mod4, mod5)


AIC(gls5, gls6, gls7, gls8, gls9, gls10, gls7_2, gls8_2, gls10_2)



# -->
# note that AIC(gls5) is differenct from AIC(mod5) due to difference of LS and REML
# gls10 is best in terms of AIC



# ----------
summary(gls10)


car::compareCoefs(gls5, gls10)



# -->
# The estimated standard errors of the fixed-effects coefficients vary more noticeably between all the models.
# This is related to the differences in the assumed residual-variance structure.

# area, ac, shikirei, are really different ..




# ------------------------------------------------------------------------------
# Compare Pearson residuals
# ------------------------------------------------------------------------------

graphics.off()

lattice::bwplot(resid(gls5, type = "pearson") ~ station, data = dat)

lattice::bwplot(resid(gls10, type = "pearson") ~ station, data = dat)




# ------------------------------------------------------------------------------
# Scale-location plots
# ------------------------------------------------------------------------------


plot(gls5, sqrt(abs(resid(., type = "pearson"))) ~ fitted(.), type = c("p", "smooth"))


plot(gls10, sqrt(abs(resid(., type = "pearson"))) ~ fitted(.), type = c("p", "smooth"))




# -->
# Square-root transformation of the absolute value of the residuals versus fitted values.
# The plots allow for detection of patterns in the residual variance.

# raw resisulas suggests a dependence between the residual variance and the mean value
# BUT THIS MAY BE AN ARTIFACT OF THE HETEROSCEDASTICITY OF THE RAW RESIDUALS.

# It might be better to look at the scale-location plot for the Pearson residuals.
# The plot does not indicate any clear trend in the residual variance

