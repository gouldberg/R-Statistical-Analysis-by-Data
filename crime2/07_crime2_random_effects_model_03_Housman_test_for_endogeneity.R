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
# Housman Test for Engdogeneity in Random Effects Model
#   - Random effects estimator are reliable under the assumption that individual characteristics (heterogeneity) are exogenous,
#     that is, they are independent with respect to the regressors in the random effects equation
#   - H0: individual random effects are exogenous
# ------------------------------------------------------------------------------


reg_within <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "within")


reg_rnd <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4.p, model = "random", random.method = "swar")



phtest(reg_within, reg_rnd)



# -->
# test rejects the null hypothesis, indicating the presence of endogeneity in the model
# (random effects equation inconsistent)




