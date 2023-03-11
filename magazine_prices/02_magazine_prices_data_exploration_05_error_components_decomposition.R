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



# we here remove duplicated rows (but "length" is changed from 6 to 7 within year) for Science Digest year 45
# also included == 1:  year 53 to 79 for palane balancing

MagazinePrices2 <- MagazinePrices %>% filter(! (year == 45 & magazine == "Science Digest" & length == 7)) %>% filter(included == 1)



# ----------
mp <- pdata.frame(MagazinePrices2, index = c("magazine", "year"), drop.index = FALSE)


head(mp)




# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------


ercomp(price ~ length + cuminf + cumsales, mp)



# -->
# theta = 0.865:  GLS estimator removes 86.5% of the individual mean
# GLS estimator is about close to pooling estimator

