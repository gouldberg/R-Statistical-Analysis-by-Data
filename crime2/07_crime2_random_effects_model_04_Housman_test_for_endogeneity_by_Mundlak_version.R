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
# Housman Test for Engdogeneity in Random Effects Model by Mudlak version, which is based on a regression model
#   - The regular Hausman test is based on homoskedasticity assumptions and can yield negative test statistics.
#     These inconveniences can be overcome by the Mundlak version of the test, which is based on a regression model of the form:
#     y(it) = delta1 + beta2 * x2(it) + alpha1 * omega(1i) + gamma2 * mean(x2(i)) + ( c(i) + e(it) )
#     where mean(x2(i)) is the time average of the independent variable
# ------------------------------------------------------------------------------


crime4.p <- pdata.frame(crime4, index = c("county", "year"))

names(crime4.p)



# ----------
# calculate time averages and merge them to data set

crime4.p <- crime4.p %>% group_by(county) %>% 
  summarize(lprbarr_m = mean(lprbarr), lprbconv_m = mean(lprbconv), lprbpris_m = mean(lprbpris), lavgsen_m = mean(lavgsen), lpolpc_m = mean(lpolpc)) %>%
  left_join(crime4.p, ., by = "county")




# ----------
# run the two regressions

formula = log(crmrte) ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + lprbarr_m + lprbconv_m + lprbpris_m + lavgsen_m + lpolpc_m


reg.ols <- plm(formula, model = "pooling", data = crime4.p)


reg.re <- plm(formula, model = "random", effect = "time", data = crime4.p)



# ----------
# retrieve standard errors

Cluster.OLS <- sqrt(diag(vcovHC(reg.ols, type = "HC0", cluster = "time")))

Conventional.RE <- sqrt(diag(vcovHC(reg.re)))

Cluster.RE <- sqrt(diag(vcovHC(reg.re, type = "HC0", cluster = "time")))



# ----------
# Perform the Mundlak test

H0 <- c("lprbarr_m = 0", "lprbconv_m = 0", "lprbpris_m = 0", "lavgsen_m = 0", "lpolpc_m = 0")


chi.OLS <- car::linearHypothesis(reg.ols, H0, vcov = vcovHC(reg.re, type = "HC0", cluster = "time"))[2,3]


chi.REconv <- car::linearHypothesis(reg.re, H0)[2,3]


chi.REclus <- car::linearHypothesis(reg.re, H0, vcov = vcovHC(reg.re, type = "HC0", cluster = "time"))[2,3]


Mundlak.test <- c(NA, chi.OLS, chi.REconv, chi.REclus)



# ----------
# collect and print the results

tab <- data.frame(coef(reg.ols), Cluster.OLS, Conventional.RE, Cluster.RE)

tab <- rbind(tab, Mundlak.test)


row.names(tab) <- c("C", "lprbarr", "lprbconv", "lprbpris", "lavgsen", "lpolpc", "lprbarr_m", "lprbconv_m", "lprbpris_m", "lavgsen_m", "lpolpc_m", "Mundlak test")

kable(tab, digits = 5, align = "c")





