
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
# Least Trimmed Squares
#   - minimize the sum of squares of the q smallest residuals
#   - This method has a high breakdown point because it can tolerate a large number of outliers depending on how q is chosen.
#   - default choice of q = [n/2] + [(p+1)/2] where [x] indicates the largest integer less than or equal to x.
# ------------------------------------------------------------------------------


set.seed(123)


ltsmod5 <- ltsreg(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
             bus_flg + ac + closet + balcony + floor + 
             closet : area + ac : area + floor : area +
             num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
             num_exchange : min_tot + num_exchange + shikirei : area + shikirei : num_exchange, data = dat)


tmp <- data.frame(coef_lm = coef(mod5), coef_ltsmod = coef(ltsmod5))


tmp



# -->
# very different coefficient






# ------------------------------------------------------------------------------
# compare:  original price,  predicted price by mod5, rlmod, ltsmod5
# ------------------------------------------------------------------------------

idx <- c(54, 166, 180, 253, 378, 405, 406, 640, 744, 761)



tmp <- data.frame(id = idx, price = dat$price[idx], mod5_price = predict(mod5)[idx], 
                  rlmod_price = predict(rlmod)[idx], ltsmod5_price = predict(ltsmod5)[idx])


tmp <- tmp %>% mutate(dif1 = mod5_price - price, dif2 = rlmod_price - price, dif3 = ltsmod5_price - price)


tmp




# ------------------------------------------------------------------------------
# Least Trimmed Squares:  bootstrapping
# ------------------------------------------------------------------------------


# nsamp = "best":  in order to avoid long computing time, we use "best", 
# but bootstrap estimates of variability will be somewhat on the high side

bcoef <- matrix(0, 1000, 21)


for(i in 1:1000){
  
  newy <- predict(ltsmod5) + residuals(ltsmod5)[sample(nrow(tmp), rep = T)]
  
  brg <- ltsreg(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
                      bus_flg + ac + closet + balcony + floor + 
                      closet : area + ac : area + floor : area +
                      num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
                      num_exchange : min_tot + num_exchange + shikirei : area + shikirei : num_exchange, data = dat, nsamp = "best")
  
  
  bcoef[i,] <- brg$coef
}


bcoef <- data.frame(bcoef)

colnames(bcoef) <- names(coef(ltsmod5))


( tmp <- apply(bcoef, 2, function(x) quantile(x, c(0.025, 0.975))) )



# ----------

library(ggplot2)


ggplot(bcoef, aes(x = area)) + geom_density() + geom_vline(xintercept = tmp[,"area"], lty = 2) + theme_bw()


ggplot(bcoef, aes(x = years)) + geom_density() + geom_vline(xintercept = tmp[,"years"], lty = 2) + theme_bw()




# -->
# really unstable model



