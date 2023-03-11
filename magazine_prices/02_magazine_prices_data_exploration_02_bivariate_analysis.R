setwd("//media//kswada//MyFiles//R//magazine_prices")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Magazine Prices
# ------------------------------------------------------------------------------

data("MagazinePrices", package = "pder")


str(MagazinePrices)


dim(MagazinePrices)


car::some(MagazinePrices)




# ------------------------------------------------------------------------------
# data exploration:  time series plot of price
# ------------------------------------------------------------------------------


library(lattice)


xyplot(price ~ year | magazine, data = MagazinePrices, pch = 20, col = "black")



# -->
# note that price have not been changed continuously. There seems to be some mechanism




# ------------------------------------------------------------------------------
# data exploration:  when price change occurred ?  (in what length, price is changed ?)
# ------------------------------------------------------------------------------


tmp <- MagazinePrices %>% filter(change == 1)


summary(tmp$length)


hist(tmp$length, breaks = seq(0, 20, by = 1))



# -->
# almost 50% is less than 3 years after previous change




# ----------
# length by year

xyplot(length ~ year, g = tmp$magazine, data = tmp, pch = 1:40)


xyplot(length ~ year, data = tmp, type = c("p", "smooth"), pch = 20, col = "black")



# by boxplot
tmp <- tmp %>% mutate(year_cat = cut(year, 7))


boxplot(length ~ year_cat, data = tmp)




# ------------------------------------------------------------------------------
# data exploration:  length by magazine
# ------------------------------------------------------------------------------

with(MagazinePrices, by(length, magazine, FUN = summary))



latticebwplot(length ~ magazine, data = MagazinePrices)






