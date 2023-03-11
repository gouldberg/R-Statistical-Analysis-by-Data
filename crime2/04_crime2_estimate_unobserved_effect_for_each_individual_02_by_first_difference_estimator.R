setwd("//media//kswada//MyFiles//R//crime2")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crime2
#  - balanced panel of 46 cities, properly sorted, which is discused by Wooldridge (2016, Secion 13.3)
# ------------------------------------------------------------------------------

# library(foreign)
# crime2 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime2.dta")
# write.table(crime2, file = "crime2.txt", row.names = F, quote = F, sep = "\t")

# library(foreign)
# crime4 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime4.dta")
# write.table(crime4, file = "crime4.txt", row.names = F, quote = F, sep = "\t")


crime2 <- read.table("crime2.txt", header = T, stringsAsFactors = FALSE, sep = "\t")

crime4 <- read.table("crime4.txt", header = T, stringsAsFactors = FALSE, sep = "\t")


str(crime2)
str(crime4)

dim(crime2)
dim(crime4)

car::some(crime2)
car::some(crime4)



# ------------------------------------------------------------------------------
# First Difference Estimator
# ------------------------------------------------------------------------------

library(plm)



# ----------
# manual calculation of first differences of crime rate
crime4.p$dcrmrte <- diff(crime4.p$crmrte)

crime4.p[1:9, c("county", "year", "crmrte", "dcrmrte")]



# ----------
# Estimate first difference model
# Note that in this specification, all variables are automatically differenced, so they have the intuitive interpretation in the level equation.

reg_fd <- plm(log(crmrte) ~ d83 + d84 + d85 + d86 + d87 + lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "fd")


# regression table with standard SE
coeftest(reg_fd)



# ------------------------------------------------------------------------------
# If not difference year dummies, we could use a pooled OLS estimator and explicitly difference the other variables
# ------------------------------------------------------------------------------

coeftest(plm(diff(log(crmrte)) ~ d83 + d84 + d85 + d86 + d87 + diff(lprbarr) + diff(lprbconv) + diff(lprbpris) + diff(lavgsen) + diff(lpolpc), data = crime4.p, model = "pooling"))


