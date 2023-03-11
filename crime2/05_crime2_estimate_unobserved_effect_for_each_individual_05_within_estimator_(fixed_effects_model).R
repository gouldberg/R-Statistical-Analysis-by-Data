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
# Within Estimator:  the difference is between a variable and its mean over all time periods
# Its advantage over the differece estimator is that it allows more than two time periods
# 
#  - The within estimator (Fixed Effects Model), requiring only minimal assumptions on the nature of heterogeneity,
#    is one of the simplest and most robust specifications in panel data econometrics and often the benchmark against which more sophisticated,
#    and possible efficient, ones are compared and judgesd in applied practice.
# ------------------------------------------------------------------------------


# model = "within" is default
reg_within <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "within", effect = "time")



# ----------
coeftest(reg_within)


# robust standard errors
coeftest(reg_within, vcovHC)




# ----------
# fixed effect for each year
fixef(reg_within)




# ------------------------------------------------------------------------------
# Within Estimator
# ------------------------------------------------------------------------------

# transofming the data by subtracting the average over time (individual) to every variable ("time-demeaning")
reg_within2 <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "within", effect = "county")



# ----------
coeftest(reg_within2)


# robust standard errors
coeftest(reg_within2, vcovHC)



# ----------
# fixed effect for each county
fixef(reg_within2)



# ----------
# fixed effect for each year
fixef(reg_within)

