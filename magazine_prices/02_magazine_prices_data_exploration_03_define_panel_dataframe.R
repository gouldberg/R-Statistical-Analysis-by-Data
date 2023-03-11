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



# ----------
# we here remove duplicated rows (but "length" is changed from 6 to 7 within year) for Science Digest year 45
# also included == 1:  year 53 to 79 for panel balancing

MagazinePrices2 <- MagazinePrices %>% filter(! (year == 45 & magazine == "Science Digest" & length == 7)) %>% filter(included == 1)




# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------


library(plm)

mp <- pdata.frame(MagazinePrices2, index = c("magazine", "year"), drop.index = FALSE)



# ----------
# This is balanced panel
pdim(mp)



# ----------
head(index(mp))



# ----------
table(index(mp)$magazine, useNA = "always")


table(index(mp)$year, useNA = "always")


table(index(mp), useNA = "ifany")


