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
# First Difference Estimator:  manual estimation by lm()
# ------------------------------------------------------------------------------

library(plm)



# ----------
# manually calcualte first differences

crime2.p$dcrmrte <- diff(crime2.p$crmrte)

crime2.p$dunem <- diff(crime2.p$unem)


crime2.p[1:6, c("id", "time", "year", "crmrte", "dcrmrte", "unem", "dunem")]



# ----------
# Estimate first difference model with lm on differenced data
# The observations for the first year with missing information are automatically dropped from the estimation sample.

coeftest(lm(dcrmrte ~ dunem, data = crime2.p))


# -->
# The results show a significantly positive relation between unemployment and crime.



# ------------------------------------------------------------------------------
# First Difference Estimator:  automatically by plm
# ------------------------------------------------------------------------------

# Estimate first difference model with plm on original data
# model = "fd" produces the FD estimator

coeftest(plm(crmrte ~ unem, data = crime2.p, model = "fd"))


