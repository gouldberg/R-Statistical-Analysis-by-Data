setwd("//media//kswada//MyFiles//R//houseprices_us")

packages <- c("dplyr", "plm")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  House Prices US
# ------------------------------------------------------------------------------

data("HousePricesUS", package = "pder")


str(HousePricesUS)


dim(HousePricesUS)



# ------------------------------------------------------------------------------
# Cross sectional correlation
# ------------------------------------------------------------------------------


php <- pdata.frame(HousePricesUS)


pdim(php)


head(index(php))



# ----------
# Cross sectional correlation assessment for the "price" (dependent variable, the house price index (1980 = 100))

cbind("rho" = pcdtest(diff(log(php$price)), test = "rho")$statistic,
      "|rho|" = pcdtest(diff(log(php$price)), test = "absrho")$statistic)



# -->
# The overall averages of rho and absolute value of rho are quite large in magnitude and very close to each other,
# indicating substantial positive correlation.




# ------------------------------------------------------------------------------
# Cross-sectional correlation matrix
# ------------------------------------------------------------------------------


regions.names <- c("New Engl", "Mideast", "Southeast", "Great Lks", "Plains", "Southwest", "Rocky Mnt", "Far West")


# cortab:  compute the cross-sectional correlation matrix
corr.table.hp <- plm::cortab(diff(log(php$price)), grouping = php$region, groupnames = regions.names)


colnames(corr.table.hp) <- substr(rownames(corr.table.hp), 1, 5)


round(corr.table.hp, 2)



# -->
# This highlights the correlation between neighboring regions and also some cases of correlation with distant ones,
# as is the case for California and some more developed stats on the East Coast.

# According to the authors (Holly et al.), this is evidence of factor-related dependence:  common shocks to technology stimulate growth in the most advanced states
# irrespective of geographic proximity.



