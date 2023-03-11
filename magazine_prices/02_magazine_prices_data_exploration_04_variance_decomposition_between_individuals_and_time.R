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
# also included == 1:  year 53 to 79 for palane balancing

MagazinePrices2 <- MagazinePrices %>% filter(! (year == 45 & magazine == "Science Digest" & length == 7)) %>% filter(included == 1)



# ----------
mp <- pdata.frame(MagazinePrices2, index = c("magazine", "year"), drop.index = FALSE)


head(mp)



# ------------------------------------------------------------------------------
# Variance decomposition between individuals and time
# ------------------------------------------------------------------------------


mod <- lm(price ~ magazine + year, data = MagazinePrices2)


summary(mod)


anova(mod)


round(anova(mod)$'Sum Sq' / sum(anova(mod)$'Sum Sq'), 3)



# ----------
# Variance decomposition
summary(mp$price)



# -->
# The variation is due to inter-individual (59.4%) and time (29.2%)

