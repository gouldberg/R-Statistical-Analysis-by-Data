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
# Pooled model:  regression for non panel data
# ------------------------------------------------------------------------------

# note we use original crime4 data (not panel data)

# OLS
reg_ols <- lm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4)


summary(reg_ols)



# ------------------------------------------------------------------------------
# Pooled model by plm() 
# ------------------------------------------------------------------------------

reg_pool <- plm(log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc, data = crime4, model = "pooling")


summary(reg_pool)


# -->
# Note that this is same by lm()



# ------------------------------------------------------------------------------
# conventional and heteroskedastic errors from OLS model
# and cluster robust standard errors from pooled model
# ------------------------------------------------------------------------------

Conventional <- sqrt(diag(vcov(reg_ols)))


Heteroskedastic <- sqrt(diag(vcovHC(reg_ols, type = "HC0")))


ClusterRobust <- sqrt(diag(vcovHC(reg_pool, type = "HC0", cluster = "time")))



# ----------
tab <- data.frame(Conventional, Heteroskedastic, ClusterRobust)

kable(tab, digits = 4, align = "c", caption = "OLS and Pooled Estimates with Alternative Errors")

